package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable
import scala.util.Random

class VectorCompressOverlapHarness(p: VectorRegFileParam, xlen: Int, elen: Int, depth: Int,
                                   chain: Boolean, mul: Boolean) extends Component {
  val io = new Bundle {
    val cmd = slave(Flow(VectorArithmeticCmd(p, xlen, true, true, mul)))
    val reads, writes = in Bits(32 bits)
    val shadow, stall = in Bool()
    val load = slave(Flow(VrfWriteCmd(p)))
    val read = slave(VrfReadPort(p))
    val full, idle, initHold, start, issue, last, reserve, flush, x2, x2Last, finalCandidate = out Bool()
    val done, saturated, progress = out Bool()
    val acceptTag, startTag, issueTag, doneTag, flushTag, x2Tag = out UInt(3 bits)
    val write = master(Flow(VrfWriteCmd(p)))
    val writeTag = out UInt(3 bits)
  }
  val q = new VectorQueue(p, xlen, depth, true, true, true, mul, chain)
  val a = new VectorArithmetic(p, xlen, elen, true, true, true, beatMul = mul, chaining = chain)
  val v = new VectorRegFile(p)
  q.push.valid := io.cmd.valid
  q.push.element := False
  q.push.elem.assignFromBits(B(0, q.push.elem.getBitsWidth bits))
  q.push.arith := io.cmd.payload
  q.push.reads := io.reads; q.push.writes := io.writes
  q.done := a.io.done; q.beatReady := a.io.ready
  a.io.cmd.valid := q.pop; a.io.cmd.payload := q.head.arith; a.io.tag := q.head.tag
  a.io.v0 := v.io.v0
  v.io.write := a.io.write
  when(io.load.valid) { v.io.write := io.load }
  for (i <- 0 until 3) v.io.reads(i).cmd := a.io.reads(i).cmd
  when(q.idle) { v.io.reads(0).cmd := io.read.cmd }
  io.read.rsp := v.io.reads(0).rsp
  if (chain) {
    q.push.shadow := io.shadow
    q.words.progress := a.io.progress; q.words.consumer := a.io.readTag; q.words.reads := a.io.candidates
    a.io.sourceReady := q.words.ready && !io.stall
    val f = new VectorReadForwarding(p)
    f.io.write := v.io.writeEvent
    f.io.older := q.words.older(a.io.writeTag, a.io.readTag) && !v.io.initHold
    for (i <- 0 until 3) {
      f.io.reads(i).cmd := a.io.reads(i).cmd; f.io.raw(i) := v.io.reads(i).rsp
      a.io.reads(i).rsp := f.io.reads(i).rsp
    }
  } else for (i <- 0 until 3) a.io.reads(i).rsp := v.io.reads(i).rsp
  io.full := q.full; io.idle := q.idle; io.initHold := v.io.initHold
  io.acceptTag := q.push.tag; io.start := q.pop; io.startTag := q.head.tag
  io.issue := a.running.pull() && a.issueFire.pull(); io.issueTag := a.lifecycle.issueTag.pull()
  io.last := a.issue.last.pull(); io.reserve := a.running.pull() && a.command.compress.pull() && a.issue.last.pull()
  io.finalCandidate := a.running.pull() && a.command.compress.pull() && a.issue.stepsDone.pull()
  io.flush := a.pack.flushPending.pull(); io.flushTag := a.pack.flushTag.pull()
  io.x2 := a.x2.valid.pull() || a.x2.last.pull(); io.x2Last := a.x2.last.pull(); io.x2Tag := a.packTags.x2.pull()
  io.write := a.io.write; io.writeTag := a.io.writeTag
  io.done := a.io.done; io.doneTag := a.io.doneTag; io.saturated := a.io.saturated
  io.progress := (if (chain) a.io.progress.valid else False)
}

// Byte and element expectations are computed before admission, independently of the pipeline.
class VectorCompressOverlapTest extends AnyFunSuite {
  case class Command(vd: Int, vs2: Int = 8, vs1: Int = 1, sew: Int = 0, vl: Int = 0, lm: Int = 1,
                     compress: Boolean = false, saturate: Boolean = false, masked: Boolean = false,
                     multiply: Boolean = false, immediate: Int = 1, vector: Boolean = false, maskLogical: Boolean = false)
  case class Owner(id: Int, tag: Int, cmd: Command, image: Array[Int], masks: mutable.Map[Int, BigInt], sat: Boolean,
                   var started: Int = -1, var first: Int = -1, var finalIssue: Int = -1, var flushed: Boolean = false)
  val lanes = Seq((128,32,32,64,1,false), (256,32,64,64,3,true),
    (128,64,64,64,3,false), (256,64,64,128,3,true), (256,64,64,128,4,false))
  for ((vlen,xlen,elen,dlen,depth,mul) <- lanes; chain <- Seq(false,true))
    test(s"compress boundaries VLEN=$vlen XLEN=$xlen ELEN=$elen DLEN=$dlen depth=$depth mul=$mul chain=$chain") {
      val p = VectorRegFileParam(vlen,dlen)
      SimConfig.workspacePath(s"build/compress-overlap/unit-$vlen-$xlen-$elen-$dlen-$depth-$mul-$chain")
        .compile(new VectorCompressOverlapHarness(p,xlen,elen,depth,chain,mul)).doSim { d =>
        val rng = new Random(0xc09 + vlen + xlen + elen + depth)
        val rb = vlen/8; val wb = dlen/8
        val image = Array.fill(32*rb)(rng.nextInt(256))
        // Explicit mask registers: zero, all ones, and a residual followed by a full word.
        for (i <- 0 until rb) { image(rb+i)=255; image(2*rb+i)=0; image(3*rb+i)=255 }
        image(3*rb)=254; image(4*rb)=0xfe
        val live = mutable.Map[Int,Owner](); val order = mutable.Queue[Owner]()
        val coverage = mutable.Map[String,Int]().withDefaultValue(0)
        var cycle=0; var sent=0; var completed=0; var checking=false; var bubble = -2
        var lastFlush: Option[Owner] = None
        d.clockDomain.fallingEdge()
        d.io.cmd.valid #= false; d.io.load.valid #= false; d.io.read.cmd.valid #= false
        d.io.read.cmd.payload #= 0; d.io.reads #= 0; d.io.writes #= 0; d.io.shadow #= false; d.io.stall #= false
        d.io.load.address #= 0; d.io.load.mask #= 0; d.io.load.data #= 0
        def tick(): Unit = {
          sleep(1)
          if (checking) {
            lastFlush.foreach { f =>
              assert(cycle<=f.finalIssue+4,"flush write missing")
              if(cycle==f.finalIssue+4)assert(d.io.write.valid.toBoolean && d.io.writeTag.toInt==f.tag)
            }
            if (d.io.full.toBoolean) coverage("full")+=1
            if (d.io.cmd.valid.toBoolean && (d.io.start.toBoolean || d.io.done.toBoolean)) coverage("simultaneous")+=1
            if (cycle == bubble+1) { assert(!d.io.issue.toBoolean,"reserved issue bubble occupied"); coverage("bubble")+=1 }
            if (d.io.start.toBoolean) {
              val o=live(d.io.startTag.toInt); o.started=cycle
              if (order.exists(x => x.id<o.id && x.started>=0)) coverage("start overlap")+=1
            }
            if (d.io.issue.toBoolean) {
              val o=live(d.io.issueTag.toInt)
              if (o.first<0) {
                o.first=cycle
                for (older <- order if older.id<o.id && older.started>=0 &&
                  (!d.io.done.toBoolean || d.io.doneTag.toInt!=older.tag)) {
                  coverage(s"${older.cmd.compress}->${o.cmd.compress}")+=1
                  if (older.cmd.compress) assert(cycle>=older.finalIssue+2)
                }
              }
              if (d.io.last.toBoolean) o.finalIssue=cycle
            }
            if (d.io.reserve.toBoolean) { bubble=cycle; assert(!d.io.start.toBoolean); coverage("reserve")+=1 }
            if (d.io.x2.toBoolean) {
              val o=live(d.io.x2Tag.toInt)
              if(d.io.x2Last.toBoolean)assert(cycle==o.finalIssue+2,"X2 terminal lost its owner")
            }
            if (d.io.flush.toBoolean) {
              assert(!d.io.x2.toBoolean && !d.io.x2Last.toBoolean)
              val o=live(d.io.flushTag.toInt); assert(o.cmd.compress && !o.flushed)
              assert(cycle==o.finalIssue+3)
              o.flushed=true; lastFlush=Some(o); coverage("flush")+=1
            }
            if (d.io.write.valid.toBoolean) {
              val o=live(d.io.writeTag.toInt); val a=d.io.write.address.toInt
              val mask=d.io.write.mask.toBigInt; val data=d.io.write.data.toBigInt
              assert(mask!=0 && o.masks.contains(a),s"unexpected write id=${o.id} word=$a")
              assert((mask & ~o.masks(a))==0,s"write mask id=${o.id} word=$a")
              for (b <- 0 until wb if mask.testBit(b))
                assert(((data>>(8*b))&255).toInt==o.image(a*wb+b),s"write data id=${o.id} word=$a byte=$b")
              o.masks(a)=o.masks(a)& ~mask
              if (o.masks(a)==0) o.masks.remove(a)
              lastFlush.filter(f=>f.id==o.id && cycle==f.finalIssue+4).foreach { _ => lastFlush=None }
            }
            if (d.io.done.toBoolean) {
              val o=order.dequeue(); assert(d.io.doneTag.toInt==o.tag)
              assert(o.masks.isEmpty,s"missing writes id=${o.id}: ${o.masks}")
              assert(d.io.saturated.toBoolean==o.sat,s"saturation id=${o.id} expected=${o.sat}")
              assert(!d.io.write.valid.toBoolean || d.io.writeTag.toInt!=o.tag)
              if(o.cmd.compress) assert(cycle==o.finalIssue+(if(o.flushed)5 else 4))
              coverage(s"sat ${o.sat}")+=1; live.remove(o.tag); completed+=1
            }
            if (d.io.progress.toBoolean) {
              // Progress carries the same writeback owner as its ALU token.
              assert(!live(d.io.writeTag.toInt).cmd.compress)
            }
          }
          d.clockDomain.risingEdge(); sleep(5); d.clockDomain.fallingEdge(); sleep(4)
          cycle+=1; assert(cycle<100000)
        }
        d.clockDomain.assertReset(); tick(); d.clockDomain.deassertReset()
        for (_ <- 0 until p.wordCount+2) tick()
        assert(!d.io.initHold.toBoolean)
        def data(at: Int): BigInt = (0 until wb).map(b=>BigInt(image(at*wb+b))<<(8*b)).reduce(_|_)
        for (at <- 0 until p.wordCount) {
          d.io.load.valid #= true; d.io.load.address #= at; d.io.load.mask #= (BigInt(1)<<wb)-1
          d.io.load.data #= data(at); tick()
        }
        d.io.load.valid #= false; checking=true
        def send(c: Command): Unit = {
          while(d.io.full.toBoolean) tick()
          val before=image.clone(); val size=1<<c.sew
          def get(r:Int,i:Int):BigInt=(0 until size).map(b=>BigInt(before(r*rb+i*size+b))<<(8*b)).reduce(_|_)
          val masks=mutable.Map[Int,BigInt]().withDefaultValue(BigInt(0)); var sat=false
          def put(i:Int,v:BigInt):Unit=for(b <- 0 until size) {
            val at=c.vd*rb+i*size+b; image(at)=((v>>(8*b))&255).toInt
            masks(at/wb)=masks(at/wb).setBit(at%wb)
          }
          if(c.compress) {
            val selected=(0 until c.vl).filter(i=>((before(c.vs1*rb+i/8)>>(i%8))&1)!=0)
            selected.zipWithIndex.foreach { case(src,dst)=>put(dst,get(c.vs2,src)) }
            val perWord=wb/size
            val finalStart=if(c.vl==0)0 else ((c.vl-1)/perWord)*perWord
            val fill=selected.count(_<finalStart)*size%wb
            val terminal=fill+selected.count(_>=finalStart)*size
            coverage(if(c.vl==0)"empty" else if(terminal==0)"terminal none" else if(terminal<wb)"terminal partial"
              else if(terminal==wb)"terminal exact" else "terminal extra")+=1
          } else if(c.maskLogical) {
            for(i <- 0 until c.vl) {
              val at=c.vd*rb+i/8
              val bit=((before(c.vs2*rb+i/8)^before(c.vs1*rb+i/8))>>(i%8))&1
              image(at)=(image(at)& ~(1<<(i%8))) | (bit<<(i%8))
            }
            for(w <- 0 until (c.vl+dlen-1)/dlen)masks(c.vd*p.beatsPerReg+w)=(BigInt(1)<<wb)-1
          } else for(i <- 0 until c.vl if !c.masked || ((before(i/8)>>(i%8))&1)!=0) {
            val b=if(c.vector)get(c.vs1,i) else BigInt(c.immediate)
            val value=if(c.multiply)get(c.vs2,i)*b else get(c.vs2,i)+b
            val limit=(BigInt(1)<<(8*size))-1
            if(c.saturate && value>limit)sat=true
            put(i,if(c.saturate)value.min(limit) else value & limit)
          }
          val tag=sent&7; assert(!live.contains(tag)); assert(d.io.acceptTag.toInt==tag)
          val o=Owner(sent,tag,c,image.clone(),masks,sat); live(tag)=o; order.enqueue(o); sent+=1
          val cmd=d.io.cmd; cmd.valid #= true; cmd.vd #= c.vd; cmd.vs2 #= c.vs2; cmd.vs1 #= c.vs1
          cmd.vl #= c.vl; cmd.vstart #= 0; cmd.sew #= c.sew; cmd.shape #= (if(c.maskLogical)VectorShape.MASK_MM else VectorShape.SINGLE)
          cmd.op #= (if(c.maskLogical)VectorAluOp.XOR else VectorAluOp.ADD)
          cmd.scalar #= c.immediate; cmd.scalarOperand #= (!c.compress && !c.vector && !c.maskLogical)
          cmd.unmasked #= !c.masked; cmd.maskData #= false; cmd.signed #= false; cmd.saturate #= c.saturate
          cmd.vxrm #= 0; cmd.nreg #= c.lm; cmd.vlmax #= (if(c.maskLogical)vlen else c.lm*vlen/(8*size))
          cmd.slide #= 0; cmd.compress #= c.compress
          if(mul) { cmd.multiply #= c.multiply; cmd.mulOp #= VectorElementOp.MUL; cmd.signedVs1 #= false; cmd.negate #= false }
          val reads=(BigInt(1)<<c.vs2) | (if(c.compress || c.vector || c.maskLogical)BigInt(1)<<c.vs1 else BigInt(0)) |
            (if(c.masked)BigInt(1) else BigInt(0)) | (if(c.maskLogical)BigInt(1)<<c.vd else BigInt(0))
          d.io.reads #= reads | (((BigInt(1)<<c.lm)-1)<<c.vs2)
          d.io.writes #= ((BigInt(1)<<c.lm)-1)<<c.vd; d.io.shadow #= c.masked
          tick(); cmd.valid #= false
        }
        def drain(): Unit = { while(!d.io.idle.toBoolean)tick(); assert(live.isEmpty) }
        val full=2*wb
        // Independent command stream, all boundary directions, tag wraps and no CSR drains.
        for (_ <- 0 until 12) {
          send(Command(16,vl=full))
          send(Command(20,vs1=3,vl=full,compress=true))
          send(Command(24,vs1=1,vl=full,compress=true))
          send(Command(28,sew=1,vl=wb-1))
        }
        // Mask logicals read old vd and preserve bits above vl, including the flush word.
        send(Command(16,vs1=3,vl=full,compress=true))
        send(Command(16,vl=dlen+3,maskLogical=true))
        // Each terminal shape and empty body, with different contexts at admission.
        for (sew <- 0 to log2Up(elen/8); vl <- Seq(0,1,(wb>>sew)-1,wb>>sew,(2*wb>>sew)-1,2*wb>>sew).distinct;
             mask <- Seq(1,2,3)) {
          send(Command(16,vs1=mask,sew=sew,vl=vl,compress=true))
          send(Command(24,vs1=if(mask==1)2 else 1,sew=(sew+1)%(log2Up(elen/8)+1),vl=1,compress=true))
          send(Command(20,sew=(sew+1)%(log2Up(elen/8)+1),vl=1))
        }
        // WAW full/partial; WAR of data and mask; explicit mask RAW; all-older A/B/C.
        for (_ <- 0 until 6) {
          send(Command(16,vs1=3,vl=full,compress=true))
          send(Command(16,vs2=12,vl=1))
          send(Command(8,vs2=16,vl=full))
          send(Command(3,vs2=12,vl=rb))
          send(Command(20,vs1=3,vl=full,compress=true))
          send(Command(16,vl=full))
          send(Command(24,vs1=1,vl=full,compress=true))
          send(Command(28,vs2=16,vs1=24,vl=full,vector=true))
        }
        // Explicit v0 mask source, then a legal v0 destination and implicit shadow use.
        send(Command(0,vs2=12,vl=rb))
        send(Command(16,vs1=0,vl=full,compress=true))
        send(Command(0,vs1=1,vl=full,compress=true))
        send(Command(20,vl=full,masked=true))
        // First-only and last-only saturation with false results at every intervening done.
        drain()
        def loadRegister(r:Int,f:Int=>Int):Unit={
          for(b <- 0 until rb)image(r*rb+b)=f(b)
          for(w <- 0 until p.beatsPerReg){val at=r*p.beatsPerReg+w
            d.io.load.valid #=true; d.io.load.address #=at; d.io.load.mask #=(BigInt(1)<<wb)-1; d.io.load.data #=data(at); tick()}
          d.io.load.valid #=false
        }
        for (lastOnly <- Seq(false,true)) {
          loadRegister(8,i=>if((i>=wb)==lastOnly)255 else 0)
          send(Command(16,vl=full,saturate=true))
          send(Command(20,vs1=1,vl=full,compress=true))
          send(Command(24,vs2=2,vl=full,saturate=true))
          send(Command(28,vl=0,saturate=true))
          drain()
        }
        loadRegister(0,_=>0)
        send(Command(16,vl=full,saturate=true,masked=true)); drain()
        if(chain) {
          send(Command(16,vs1=1,vl=full,compress=true))
          while(!d.io.finalCandidate.toBoolean)tick()
          d.io.stall #=true
          for(_ <- 0 until 3){tick(); assert(!d.io.reserve.toBoolean)}
          d.io.stall #=false; coverage("stalled final")+=1
          send(Command(20,vl=1)); drain()
        }
        // No following queued command: readiness must recover after a gap.
        send(Command(16,vs1=1,vl=0,compress=true)); drain()
        for(_ <- 0 until 7)tick()
        send(Command(20,vs1=2,vl=full,compress=true))
        if(mul) for(_ <- 0 until 4) {
          send(Command(24,vl=wb,multiply=true))
          send(Command(16,vs1=1,vl=full,compress=true))
          send(Command(28,vl=1,multiply=true))
        }
        drain(); assert(sent==completed && sent>8)
        for(key <- Seq("start overlap","false->true","true->false","true->true","bubble","reserve","flush","sat true","sat false","full","simultaneous",
          "empty","terminal none","terminal partial","terminal exact","terminal extra"))
          assert(coverage(key)>0,s"unobserved $key")
        assert(coverage("bubble")==coverage("reserve"))
        if(chain)assert(coverage("stalled final")>0)
        // Every physical byte, including disabled tails, masks and source registers.
        for(at <- 0 until p.wordCount) {
          d.io.read.cmd.valid #=true; d.io.read.cmd.payload #=at; tick()
          assert(d.io.read.rsp.toBigInt==data(at),s"physical readback word=$at")
        }
        println(s"compress overlap commands=$sent coverage=${coverage.toSeq.sorted.mkString(",")}")
      }
    }
}

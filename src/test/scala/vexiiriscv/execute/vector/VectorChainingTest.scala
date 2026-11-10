package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random
import scala.collection.mutable

class VectorChainingHarness(p: VectorRegFileParam, xlen: Int, elen: Int, depth: Int, chain: Boolean, mul: Boolean) extends Component {
  val io = new Bundle {
    val cmd = slave(Flow(VectorArithmeticCmd(p,xlen,true,true,mul)))
    val reads, writes = in Bits(32 bits)
    val shadow = in Bool()
    val load = slave(Flow(VrfWriteCmd(p)))
    val full, idle, initHold = out Bool()
    val read = slave(VrfReadPort(p))
    val issued, completed, started, writing = out Bool()
    val issueTag, doneTag, startTag = out UInt(3 bits)
  }
  val q = new VectorQueue(p,xlen,depth,true,true,true,mul,chain)
  val a = new VectorArithmetic(p,xlen,elen,true,true,true,beatMul=mul,chaining=chain)
  val v = new VectorRegFile(p)
  q.push.valid := io.cmd.valid
  q.push.element := False; q.push.elem.assignFromBits(B(0,q.push.elem.getBitsWidth bits))
  q.push.arith := io.cmd.payload; q.push.reads := io.reads; q.push.writes := io.writes
  q.done := a.io.done; q.beatReady := a.io.ready
  a.io.cmd.valid := q.pop; a.io.cmd.payload := q.head.arith; a.io.tag := q.head.tag
  a.io.v0 := v.io.v0
  v.io.write := a.io.write
  when(io.load.valid) { v.io.write := io.load }
  for(i <- 0 until 3) v.io.reads(i).cmd := a.io.reads(i).cmd
  when(q.idle) { v.io.reads(0).cmd := io.read.cmd }
  io.read.rsp := v.io.reads(0).rsp
  if(chain) {
    q.push.shadow := io.shadow
    q.words.progress := a.io.progress; q.words.consumer := a.io.readTag; q.words.reads := a.io.candidates
    a.io.sourceReady := q.words.ready
    val f = new VectorReadForwarding(p)
    f.io.write := v.io.writeEvent
    f.io.older := q.words.older(a.io.writeTag,a.io.readTag) && !v.io.initHold
    for(i <- 0 until 3) {
      f.io.reads(i).cmd := a.io.reads(i).cmd; f.io.raw(i) := v.io.reads(i).rsp
      a.io.reads(i).rsp := f.io.reads(i).rsp
    }
  } else for(i <- 0 until 3) a.io.reads(i).rsp := v.io.reads(i).rsp
  io.full := q.full; io.idle := q.idle; io.initHold := v.io.initHold
  io.issued := a.io.reads.map(_.cmd.valid).orR
  io.completed := a.io.done; io.started := q.pop; io.writing := a.io.write.valid
  io.issueTag := RegNextWhen(q.head.tag,q.pop) init(0); io.doneTag := a.io.doneTag; io.startTag := q.head.tag
}

class VectorChainingTest extends AnyFunSuite {
  for(dlen <- Seq(64,128)) test(s"registered byte forwarding DLEN=$dlen") {
    val p=VectorRegFileParam(256,dlen)
    SimConfig.workspacePath(s"build/beat-chaining/forward-$dlen").compile(new VectorReadForwarding(p)).doSim { d =>
      d.clockDomain.forkStimulus(10)
      val rng=new Random(819)
      for(n <- 0 until 500) {
        val data=BigInt(dlen,rng)
        val mask=if(n%16==0) BigInt(0) else if(n%16==1) (BigInt(1)<<p.beatBytes)-1 else BigInt(p.beatBytes,rng)
        val addr=rng.nextInt(8)
        val older=rng.nextBoolean();val valid=rng.nextBoolean()
        d.io.write.valid #=valid;d.io.write.address #=addr;d.io.write.data #=data;d.io.write.mask #=mask;d.io.older #=older
        val raw=Seq.fill(3)(BigInt(dlen,rng))
        val want=(0 until 3).map { i =>
          val ra=if(n%3==i)addr else rng.nextInt(8);val rv=rng.nextBoolean()
          d.io.reads(i).cmd.valid #=rv;d.io.reads(i).cmd.payload #=ra
          var v=raw(i)
          if(older&&valid&&rv&&ra==addr) for(b <- 0 until p.beatBytes if mask.testBit(b))
            v=(v & ~(BigInt(255)<<(8*b))) | (data & (BigInt(255)<<(8*b)))
          v
        }
        d.clockDomain.waitSampling();sleep(1)
        // Change the live bus after the edge: the response must use the captured write.
        d.io.write.data #=BigInt(dlen,rng);d.io.write.mask #=0
        for(i <- 0 until 3)d.io.raw(i) #=raw(i)
        sleep(1)
        for(i <- 0 until 3)assert(d.io.reads(i).rsp.toBigInt==want(i))
      }
    }
  }
  for((xlen,elen,dlen,depth,mul) <- Seq((32,32,64,1,false),(64,64,128,3,true),(64,64,64,3,true)); chain <- Seq(false,true))
    test(s"integrated XLEN=$xlen ELEN=$elen DLEN=$dlen depth=$depth mul=$mul chain=$chain") {
      val p=VectorRegFileParam(256,dlen)
      SimConfig.workspacePath(s"build/beat-chaining/integrated-$xlen-$dlen-$depth-$chain").compile(new VectorChainingHarness(p,xlen,elen,depth,chain,mul)).doSim { d =>
        d.clockDomain.forkStimulus(10)
        d.io.cmd.valid #=false;d.io.load.valid #=false;d.io.read.cmd.valid #=false
        d.io.load.address #=0;d.io.load.data #=0;d.io.load.mask #=0
        d.io.read.cmd.payload #=0;d.io.reads #=0;d.io.writes #=0;d.io.shadow #=false
        val starts=mutable.Map[Int,Int]();val first=mutable.Map[Int,Int]();var cycle=0;var finishes=0
        def tick():Unit={
          sleep(1)
          if(d.io.started.toBoolean)starts(d.io.startTag.toInt)=cycle
          if(d.io.issued.toBoolean && !first.contains(d.io.issueTag.toInt))first(d.io.issueTag.toInt)=cycle
          if(d.io.completed.toBoolean){ finishes+=1; first.remove(d.io.doneTag.toInt) }
          d.clockDomain.waitSampling();sleep(1);cycle+=1;assert(cycle<30000)
        }
        for(_ <- 0 until 5)tick()
        while(d.io.initHold.toBoolean)tick()
        val bytes=Array.tabulate(32*p.vlen/8)(i=>(i*37+9)&255)
        def get(reg:Int,i:Int,w:Int):BigInt=(0 until w/8).map(j=>BigInt(bytes(reg*p.vlen/8+i*w/8+j))<<(8*j)).reduce(_|_)
        def put(reg:Int,i:Int,w:Int,v:BigInt):Unit=for(j <- 0 until w/8)bytes(reg*p.vlen/8+i*w/8+j)=((v>>(8*j))&255).toInt
        for(at <- 0 until p.wordCount){
          d.io.load.valid #=true;d.io.load.address #=at
          d.io.load.mask #=(BigInt(1)<<p.beatBytes)-1
          d.io.load.data #=(0 until p.beatBytes).map(j=>BigInt(bytes(at*p.beatBytes+j))<<(8*j)).reduce(_|_);tick()
        }
        d.io.load.valid #=false
        val begin=cycle
        var cmds=0
        def send(shape:Int,op:Int,vd:Int,vs2:Int,vs1:Int,w:Int,vl:Int,masked:Boolean=false,multiply:Boolean=false):Unit={
          while(d.io.full.toBoolean)tick()
          val c=d.io.cmd;c.valid #=true;c.vd #=vd;c.vs2 #=vs2;c.vs1 #=vs1;c.vl #=vl;c.vstart #=0;c.sew #=log2Up(w/8)
          c.shape #=shape;c.op #=op;c.scalar #=1;c.scalarOperand #=true;c.unmasked #= !masked;c.maskData #=false
          c.signed #=false;c.saturate #=false;c.vxrm #=0;c.nreg #=1;c.vlmax #=(8*p.vlen/w);c.slide #=0;c.compress #=false
          if(mul){c.multiply #=multiply;c.mulOp #=VectorElementOp.MACC;c.signedVs1 #=false;c.negate #=false}
          d.io.reads #=((BigInt(1)<<vs2)|(if(multiply)BigInt(1)<<vd else BigInt(0)))
          d.io.writes #=((BigInt(255)<<vd)&((BigInt(1)<<32)-1));d.io.shadow #=masked
          for(i <- 0 until vl if !masked || ((bytes(i/8)>>(i%8))&1)!=0){
            val a=get(vs2,i,if(shape==VectorShape.NARROW)w*2 else if(shape==VectorShape.EXT2)w/2 else w)
            val value=if(multiply)get(vd,i,w)+a else if(shape==VectorShape.NARROW)a>>1 else if(shape==VectorShape.EXT2)a else a+1
            put(vd,i,w,value)
          }
          tick();c.valid #=false;cmds+=1
        }
        for(n <- 0 until 12)send(VectorShape.SINGLE,VectorAluOp.ADD,8,8,0,8,8*p.vlen/8)
        for(n <- 0 until 5){
          send(VectorShape.EXT2,VectorAluOp.ADD,16,8,0,16,4*p.vlen/16)
          send(VectorShape.SINGLE,VectorAluOp.ADD,16,16,0,16,4*p.vlen/16,masked=true)
          send(VectorShape.NARROW,VectorAluOp.SRL,8,16,0,8,4*p.vlen/16-1)
          send(VectorShape.SINGLE,VectorAluOp.ADD,8,8,0,8,4*p.vlen/16-1)
        }
        if(mul) {
          for(n <- 0 until 12)send(VectorShape.SINGLE,VectorAluOp.ADD,24,24,0,64,2*p.vlen/64,multiply=true)
          for(w <- Seq(8,64); n <- 0 until 12)
            send(VectorShape.SINGLE,VectorAluOp.ADD,24,24,0,w,p.dlen/w,multiply=true)
        }
        while(!d.io.idle.toBoolean)tick()
        assert(finishes==cmds)
        println(s"CHAIN_CYCLES xlen=$xlen dlen=$dlen chain=$chain cycles=${cycle-begin} commands=$cmds")
        for(at <- 0 until p.wordCount){
          d.io.read.cmd.valid #=true;d.io.read.cmd.payload #=at;tick()
          val want=(0 until p.beatBytes).map(j=>BigInt(bytes(at*p.beatBytes+j))<<(8*j)).reduce(_|_)
          assert(d.io.read.rsp.toBigInt==want,s"word=$at got=${d.io.read.rsp.toBigInt.toString(16)} want=${want.toString(16)}")
        }
      }
    }
}

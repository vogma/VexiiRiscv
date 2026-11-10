package vexiiriscv.execute.vector

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import scala.collection.mutable
import scala.util.Random

class VectorBeatArithmeticTest extends AnyFunSuite {
  def mask(w: Int): BigInt = (BigInt(1) << w) - 1
  def sx(v: BigInt, w: Int, sign: Boolean): BigInt = if (sign && v.testBit(w-1)) v-(BigInt(1)<<w) else v
  for ((xlen, elen, dlen) <- Seq((32,32,64), (64,32,64), (32,64,64), (64,64,64), (64,64,128)); overlap <- Seq(false,true)) {
    test(s"word multiply XLEN=$xlen ELEN=$elen DLEN=$dlen overlap=$overlap") {
      val p = VectorRegFileParam(vlen=256, dlen=dlen)
      SimConfig.workspacePath(s"build/beat-mul/unit-$xlen-$elen-$dlen-$overlap").compile(new VectorArithmetic(p,xlen,elen,overlap=overlap,beatMul=true)).doSim { dut =>
        dut.clockDomain.forkStimulus(10)
        val rng = new Random(2917)
        val memory = Array.fill(32*p.beatsPerReg)(BigInt(dlen,rng))
        val reference = memory.clone()
        val expected = mutable.Map[Int,Array[BigInt]]()
        val writes = mutable.ArrayBuffer[(Int,Int)]()
        val completions = mutable.ArrayBuffer[Int]()
        var cycle=0; var tag=0
        val v0 = BigInt("5555555555555555555555555555555555555555555555555555555555555555",16)
        dut.io.cmd.valid #= false; dut.io.v0 #= v0
        if(overlap) dut.io.tag #= 0
        dut.io.reads.foreach(_.rsp #= 0)
        def tick(): Unit = {
          sleep(1)
          val data = dut.io.reads.map(r => if(r.cmd.valid.toBoolean) memory(r.cmd.payload.toInt) else BigInt(0))
          val writing = dut.io.write.valid.toBoolean
          if(writing) {
            val address=dut.io.write.address.toInt; val enables=dut.io.write.mask.toBigInt
            val value=dut.io.write.data.toBigInt; val wt=if(overlap) dut.io.writeTag.toInt else tag
            val want=expected(wt)(address)
            for(j <- 0 until dlen/8 if enables.testBit(j)) {
              assert(((value>>(8*j))&255)==((want>>(8*j))&255),s"tag=$wt address=$address byte=$j")
              memory(address)=(memory(address) & ~(BigInt(255)<<(8*j)))|(value&(BigInt(255)<<(8*j)))
            }
            writes += ((wt,cycle))
          }
          if(dut.io.done.toBoolean) {
            val dt=if(overlap) dut.io.doneTag.toInt else tag
            assert(!completions.contains(dt),s"duplicate completion $dt")
            completions += dt
          }
          dut.clockDomain.waitSampling(); sleep(1)
          dut.io.reads.zip(data).foreach{case(r,v)=>r.rsp #= v}
          cycle+=1
          assert(cycle<30000,"multiply command did not complete")
        }
        for(_ <- 0 until 5) tick()
        def read(image:Array[BigInt],reg:Int,i:Int,w:Int):BigInt = {
          val bit=reg*p.vlen+i*w
          (image(bit/dlen)>>(bit%dlen))&mask(w)
        }
        def put(image:Array[BigInt],reg:Int,i:Int,w:Int,value:BigInt):Unit = {
          val bit=reg*p.vlen+i*w;val at=bit/dlen;val shift=bit%dlen
          image(at)=(image(at) & ~(mask(w)<<shift))|((value&mask(w))<<shift)
        }
        val ops=Seq(VectorElementOp.MUL,VectorElementOp.MULH,VectorElementOp.WMUL,VectorElementOp.MACC,
          VectorElementOp.MADD,VectorElementOp.WMACC,VectorElementOp.SMUL)
        for(sew <- 0 to log2Up(elen/8); op <- ops if !(sew==3&&Set(1,2,5,6)(op)) &&
            !(8<<sew == elen && Set(2,5)(op)); scalar <- Seq(false,true); masked <- Seq(false,true)) {
          val w=8<<sew; val wide=Set(2,5)(op); val dst=if(wide)2*w else w
          val vl=if(rng.nextInt(8)==0)0 else 4*p.vlen/dst-rng.nextInt(dlen/dst)
          val sa=rng.nextBoolean()||op==6;val sb=rng.nextBoolean()||op==6
          val neg=Set(3,4)(op)&&rng.nextBoolean();val rm=rng.nextInt(4)
          val scalarBits=mask(xlen)-16
          var saturation=false
          for(i <- 0 until vl if !masked||v0.testBit(i)) {
            val a=read(reference,8,i,w);val b=if(scalar) sx(scalarBits,xlen,true)&mask(w) else read(reference,16,i,w)
            val old=read(reference,24,i,dst)
            val prod=sx(if(op==4)old else a,w,sa)*sx(b,w,sb)
            var value=(if(Set(3,5)(op))old else if(op==4)a else BigInt(0))+(if(neg)-prod else prod)
            if(op==1)value=prod>>w
            if(op==6){
              val shifted=prod>>(w-1);val half=prod.testBit(w-2);val sticky=(prod&mask(w-2))!=0
              val inc=rm match{case 0=>half;case 1=>half&&(sticky||shifted.testBit(0));case 2=>false;case 3=> !shifted.testBit(0)&&(half||sticky)}
              val rounded=shifted+(if(inc)1 else 0)
              saturation ||= rounded>mask(w-1)
              value=rounded.min(mask(w-1))
            }
            put(reference,24,i,dst,value)
          }
          expected(tag)=reference.clone()
          val c=dut.io.cmd
          c.valid #= true;c.vd #=24;c.vs2 #=8;c.vs1 #=16;c.vl #=vl;c.vstart #=0;c.sew #=sew
          c.shape #=(if(wide)VectorShape.WIDE_VV else VectorShape.SINGLE);c.op #=VectorAluOp.ADD
          c.scalar #=scalarBits;c.scalarOperand #=scalar;c.unmasked #= !masked;c.maskData #=false
          c.signed #=sa;c.signedVs1 #=sb;c.negate #=neg;c.mulOp #=op;c.multiply #=true
          c.saturate #=false;c.vxrm #=rm;c.nreg #=1
          if(overlap)dut.io.tag #=tag
          val start=cycle;val before=completions.size
          tick();c.valid #=false
          while(completions.size==before)tick()
          assert(dut.io.saturated.toBoolean==saturation,s"saturation op=$op sew=$sew")
          assert(memory.sameElements(reference),s"untouched bytes changed op=$op sew=$sew")
          val times=writes.filter(_._1==tag).map(_._2)
          val expectedStride=if(sew==3)2 else 1
          if(!masked&&times.size>1)assert(times.sliding(2).forall(t=>t(1)-t(0)==expectedStride))
          assert(cycle-start<= (vl*dst+dlen-1)/dlen*expectedStride+10)
          expected.remove(tag);writes.clear();completions.clear();tag=(tag+1)&7
          tick()
        }
        if(overlap) {
          // Independent commands use the same destination: the final pass releases
          // the next command while older words are still in the pipeline.
          val sew=if(elen==64)3 else 2;val w=8<<sew;val vl=2*p.vlen/w
          val stride=if(sew==3)2 else 1
          val starts=mutable.ArrayBuffer[Int]()
          completions.clear();writes.clear()
          val c=dut.io.cmd
          c.vd #=24;c.vs2 #=8;c.vs1 #=16;c.vl #=vl;c.sew #=sew;c.vstart #=0
          c.shape #=VectorShape.SINGLE;c.op #=VectorAluOp.ADD;c.mulOp #=VectorElementOp.MUL
          c.multiply #=true;c.scalarOperand #=true;c.unmasked #=true;c.maskData #=false
          c.signed #=false;c.signedVs1 #=false;c.negate #=false;c.saturate #=false;c.vxrm #=0;c.nreg #=1
          for(n <- 0 until 4) {
            while(!dut.io.ready.toBoolean)tick()
            val scalar=13+n
            for(i <- 0 until vl)put(reference,24,i,w,read(reference,8,i,w)*scalar)
            expected(tag)=reference.clone();c.scalar #=scalar;dut.io.tag #=tag
            starts+=cycle;c.valid #=true;tick();c.valid #=false;tag=(tag+1)&7
          }
          while(dut.io.busy.toBoolean)tick()
          assert(completions.size==4)
          assert(starts.sliding(2).forall(t=>t(1)-t(0)==2*p.vlen/dlen*stride))
          assert(memory.sameElements(reference))
        }
      }
    }
  }
}

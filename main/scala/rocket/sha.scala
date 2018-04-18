package rocket

import rocket.ALU.SZ_ALU_FN
//import Node._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import uncore.devices._
import uncore.util.CacheName
import uncore.constants._
import uncore.tilelink2._
import util._
import Chisel.ImplicitConversions._
import Chisel._
import config._

class ShaReq(dataBits: Int) extends Bundle {
  val in1 = Bits(width = dataBits)
  val in2 = Bits(width = dataBits)
  val cr = Bool()
  val old_hash = Bits(width = 24)
  override def cloneType = new ShaReq(dataBits).asInstanceOf[this.type]
}

class ShaResp(dataBits: Int) extends Bundle {
  val data = Bits(width = dataBits)
  val cr = Bool()
  val old_hash = Bits(width = 24)
  override def cloneType = new ShaResp(dataBits).asInstanceOf[this.type]
}

class ShaIO(dataBits: Int) extends Bundle {
  val req = Decoupled(new ShaReq(dataBits)).flip
  val kill = Bool(INPUT)
  val resp = Decoupled(new ShaResp(dataBits))
}

class Sha(implicit p: Parameters) extends CoreModule()(p) {
  val io = new ShaIO(64)
  val stage_num = 1
  val round_num = 20 //2*4+12

  val dpath = Module(new DpathModule(64, 1))
  dpath.io.begin := false.B
  dpath.io.end   := false.B
  dpath.io.stage := 0.U

  val cycle_n = Reg(init = UInt(0,log2Ceil(stage_num*round_num)))

  val cr = Reg(Bool())
  val old_hash = Reg(UInt())

  val s_idle:: s_absorb :: s_squeeze :: Nil = Enum(UInt(), 3)
  // absorb is absorb(1 cycle) and work
  // s_squeeze is suqeeze and wait to output
  val state = Reg(init=s_idle)

  state := state
  cycle_n := cycle_n + UInt(1)

  switch(state) {
    is(s_idle) {
      cycle_n := 0.U
      when(io.req.fire()) {
        state := s_absorb
        dpath.io.begin := true.B
        cr := io.req.bits.cr
        old_hash := io.req.bits.old_hash
      }
    }
    is(s_absorb) {
      when(cycle_n >= UInt(round_num*stage_num)-1.U) {
        state := s_squeeze
      }
    }
    is(s_squeeze) {
      dpath.io.end := true.B  //hold the value
      when(io.resp.fire()) {
        state := s_idle
      }
    }
  }


  when(io.kill){
    state:=s_idle
    cycle_n := 0.U
  }

  dpath.io.in1 := io.req.bits.in1
  dpath.io.in2 := io.req.bits.in2
  dpath.io.round := cycle_n >> log2Ceil(stage_num)

  io.req.ready := state===s_idle
  io.resp.valid := state===s_squeeze
  io.resp.bits.data := dpath.io.out
  io.resp.bits.cr := cr
  io.resp.bits.old_hash := old_hash


}


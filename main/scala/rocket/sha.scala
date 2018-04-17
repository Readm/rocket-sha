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

  val dpath = Module(new DpathModule(64, 1))
  dpath.io.begin := false.B
  dpath.io.end   := false.B
  dpath.io.stage := 0.U

  val round = Reg(init = UInt(0,5))

  val cr = Reg(Bool())
  val old_hash = Reg(UInt())

  val s_idle:: s_absorb :: s_squeeze :: Nil = Enum(UInt(), 3)
  // absorb is absorb(1 cycle) and work
  // s_squeeze is suqeeze and wait to output
  val state = Reg(init=s_idle)

  state := state

  switch(state) {
    is(s_idle) {
      when(io.req.fire()) {
        state := s_absorb
        round := 1.U

        dpath.io.in1 := io.req.bits.in1
        dpath.io.in2 := io.req.bits.in2
        dpath.io.begin := true.B
        dpath.io.round := 0.U

        cr := io.req.bits.cr
        old_hash := io.req.bits.old_hash
        // input here
      }
    }
    is(s_absorb) {
      when(round < UInt(24)) {
        round := round + UInt(1)

        dpath.io.round := round

      }.otherwise {
        round := UInt(0)
        state := s_squeeze
        dpath.io.end := false.B
      }
    }
    is(s_squeeze) {
      dpath.io.end := false.B  //hold the value
      when(io.resp.fire()) {
        state := s_idle
      }
    }
  }


  when(io.kill){state:=s_idle}

  io.req.ready := state===s_idle
  io.resp.valid := state===s_squeeze
  io.resp.bits.data := dpath.io.out
  io.resp.bits.cr := cr
  io.resp.bits.old_hash := old_hash


}


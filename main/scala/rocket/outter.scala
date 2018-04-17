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

class Outter(implicit p: Parameters) extends CoreModule()(p) {
  val io = new ShaIO(64)
  /*val io  = new Bundle {
    val in0 = UInt(INPUT, 64)
    val in1 = UInt(INPUT, width = 64)
    val ready = Bool(INPUT)
    val kill  = Bool(INPUT)

    val out = UInt(width = 64)
    val valid = Bool(OUTPUT)
    val busy  = Bool(OUTPUT)
  }*/

  val sha3_init = Reg(init=Bool(false))
  val aindex = Reg(init = UInt(0,5))
  val sum = Reg(UInt()) // temp out = in0 + in1
  val cr = Reg(Bool())
  val old_hash = Reg(UInt())

  val s_idle:: s_absorb :: s_squeeze :: Nil = Enum(UInt(), 3)
  val state = Reg(init=s_idle)

  state := state

  switch(state) {
    is(s_idle) {
      when(io.req.fire()) {
        state := s_absorb
        sum := io.req.bits.in1 ^ io.req.bits.in2
        cr := io.req.bits.cr
        old_hash := io.req.bits.old_hash
        // input here
      }
    }
    is(s_absorb) {
      when(aindex < UInt(24)) {
        aindex := aindex + UInt(1)
      }.otherwise {
        aindex := UInt(0)
        sha3_init := true
        state := s_squeeze
      }
    }
    is(s_squeeze) {
      when(io.resp.fire()) {
        state := s_idle
        sha3_init:= false
      }
    }
  }


  when(io.kill){state:=s_idle}

  io.req.ready := state===s_idle
  io.resp.valid := state===s_squeeze
  io.resp.bits.data := sum
  io.resp.bits.cr := cr
  io.resp.bits.old_hash := old_hash


}


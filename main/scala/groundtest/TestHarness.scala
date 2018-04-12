package groundtest

import Chisel._
import diplomacy._
import config._
import rocketchip._
import util._

class TestHarness(q: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }
  implicit val p = q

  val dut = Module(LazyModule(new GroundTestTop).module)
  io.success := dut.io.success

  if (dut.io.mem_axi4.nonEmpty) {
    val memSize = p(ExtMem).size
    require(memSize % dut.io.mem_axi4.size == 0)
    for (axi4 <- dut.io.mem_axi4) {
      Module(LazyModule(new SimAXIMem(memSize / dut.io.mem_axi4.size)).module).io.axi4 <> axi4
    }
  }
}

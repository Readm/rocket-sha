//see LICENSE for license
//authors: Colin Schmidt, Adam Izraelevitz
package rocket

import Chisel._

object RHOPI {

  val piln = Array(
     0, 18, 21, 19, 14,
    10, 3,  24, 13,  22, 
    7, 5,  4, 12, 9, 
    11, 16, 15,  2, 6, 
    17, 8, 23,  20,  1 
  )

  val tri = Array(
      0, 36,  3, 41, 18,
      1, 44, 10, 45,  2,
     62,  6, 43, 15, 61,
     28, 55, 25, 21, 56,
     27, 20, 39,  8, 14
  )
}
object IOTA {
val round_const = Vec(
  Bits("h0000000000000001",64),
  Bits("h0000000000008082",64),
  Bits("h800000000000808a",64),
  Bits("h8000000080008000",64),
  Bits("h000000000000808b",64),
  Bits("h0000000080000001",64),
  Bits("h8000000080008081",64),
  Bits("h8000000000008009",64),
  Bits("h000000000000008a",64),
  Bits("h0000000000000088",64),
  Bits("h0000000080008009",64),
  Bits("h000000008000000a",64),
  Bits("h000000008000808b",64),
  Bits("h800000000000008b",64),
  Bits("h8000000000008089",64),
  Bits("h8000000000008003",64),
  Bits("h8000000000008002",64), 
  Bits("h8000000000000080",64), 
  Bits("h000000000000800a",64),
  Bits("h800000008000000a",64),
  Bits("h8000000080008081",64),
  Bits("h8000000000008080",64),
  Bits("h0000000080000001",64),
  Bits("h8000000080008008",64),
  Bits("h0000000000000000",64)  )
}

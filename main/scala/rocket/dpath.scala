//see LICENSE for license
//authors: Colin Schmidt, Adam Izraelevitz
package rocket

import Chisel._
//import Node._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class DpathModule(val W: Int, val S: Int) extends Module {
  //constants
  val r = 256
  val c = 144
  //val round_size_words = c/W
  val rounds = 24 //12 + 2l
  val state_W = 4
  //val hash_size_words = 256/W
  //val bytes_per_word = W/8

  val io = new Bundle { 
    //val absorb = Bool(INPUT)
    //val init   = Bool(INPUT)
    //val write  = Bool(INPUT)

    //val aindex = UInt(INPUT,width=log2Up(hash_size_words))
    //val message_in = Bits(INPUT, width = W)
    //val hash_out = Vec(hash_size_words, Bits(OUTPUT, width = W))

    val in1 = UInt(INPUT, width=W)
    val in2 = UInt(INPUT, width=W)
    val begin = Bool(INPUT)
    val end = Bool(INPUT)
    val round  = UInt(INPUT,width=5)
    val stage  = UInt(INPUT,width=log2Up(S))
    val out = UInt(OUTPUT, width=W)
  }

  val state = Reg(Vec(5*5, Bits(0, width = 4)))

  //submodules
  val theta = Module(new ThetaModule(state_W)).io
  val rhopi = Module(new RhoPiModule(state_W)).io
  val chi   = Module(new ChiModule(state_W)).io
  val iota  = Module(new IotaModule(state_W))

  //default
  for (i <- 0 until 25)
    theta.state_i(i) := UInt(0, width=state_W)
  iota.io.round     := UInt(0)

  //connect submodules to each other
    if(S == 1){
      theta.state_i := state
      rhopi.state_i <> theta.state_o
      chi.state_i   <> rhopi.state_o
      iota.io.state_i  <> chi.state_o
      state         := iota.io.state_o
    }
    if(S == 2){
      //stage 1
      theta.state_i := state
      rhopi.state_i <> theta.state_o
      
      //stage 2
      chi.state_i   := state
      iota.io.state_i  <> chi.state_o
    }
    if(S == 4){
      //stage 1
      theta.state_i := state
      //stage 2
      rhopi.state_i := state
      //stage 3
      chi.state_i   := state
      //stage 3
      iota.io.state_i  := state
  }

  iota.io.round    := io.round
  
  //try moving mux out
  switch(io.stage){
      is(UInt(0)){
        if(S == 1){
          state := iota.io.state_o
        }else if(S == 2){
          state := rhopi.state_o
        }else if(S == 4){
          state := theta.state_o
        }
      }
      is(UInt(1)){
        if(S == 2){
          state := iota.io.state_o
        }else if(S == 4){
          state := rhopi.state_o
        }
      }
      is(UInt(2)){
        if(S == 4){
          state := chi.state_o
        }
      }
      is(UInt(3)){
        if(S == 4){
          state := iota.io.state_o
        }
      }
  }

  when(io.begin){
    state := state // when not begin or end, the state is always changing
    state(0) := io.in1(15,0)
    state(1) := io.in1(31,16)
    state(2) := io.in1(47,32)
    state(3) := io.in1(63,48)
    state(4) := io.in2(15,0)
    state(5) := io.in2(31,16)
    state(6) := io.in2(47,32)
    state(7) := io.in2(63,48)
    for (i <- 8 until 25)
      state(i) := 0.U
  }

  when(io.end){
    state := state
    io.out := Cat(state(0), state(2), state(4), state(6))
  }

}

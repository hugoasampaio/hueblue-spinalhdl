package bpskscala

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


object MyTopLevelSim extends App {
  
  Config.sim.compile(FirFilter()).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)
  }
}

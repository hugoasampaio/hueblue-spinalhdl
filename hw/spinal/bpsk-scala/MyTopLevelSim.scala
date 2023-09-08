package bpskscala

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.sim.{FlowDriver, FlowMonitor, ScoreboardInOrder}


object MyTopLevelSim extends App {
  
  Config.sim.compile(FirFilter()).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10000)
    for( i <- 0 to 5) {
      dut.clockDomain.waitSampling()
    }
  }
}

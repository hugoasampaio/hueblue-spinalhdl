package bpskscala

import spinal.core._
import spinal.core.sim._

object MyTopLevelSim extends App {
  Config.sim.compile(Convolution(43)).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)
    

  }
}

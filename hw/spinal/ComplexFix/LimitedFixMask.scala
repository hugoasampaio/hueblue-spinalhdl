package LimitedFixMask

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class LimitedFixMask() extends Component {
  val io = new Bundle {
    val setter = slave Flow (UInt(15 bits))
    val mask = out Bits(15 bits)
  }
  io.mask := B"15'h7fff"
  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      whenIsActive {
        when(io.setter.valid) {
          io.mask := io.setter.toReg().asBits
        }
        goto(idle)
      }
    }
  }
}
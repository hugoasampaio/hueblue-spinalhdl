package bpskscala

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// Hardware definition
case class Convolution[I <: AFix, O <: AFix](inputDataType:  HardType[I], outputDataType:  HardType[O], width: Int) extends Component {
  val io = new Bundle {
    var signal = slave  Flow(inputDataType)
    var result = master Flow(outputDataType)
  }
  //val mul = Vec(Reg(inputDataType), width)
  //val signal_hist = Vec(Reg(inputDataType), width)
  val mul = Vec.fill(width)(Reg(inputDataType()))
  val signal_hist = Vec.fill(width)(Reg(inputDataType()))
  signal_hist.foreach( _ := 0.0) //TODO: init signal_hist only on instantiation
  val coeff = Vec(AFix.S(2 exp, -12 exp), 43)
  coeff(0) := 0.01897049
  coeff(0).raw.msb := True
  coeff(1) := 0.01842133
  coeff(1).raw.msb := True
  coeff(2) := 0.00954316
  coeff(2).raw.msb := True
  coeff(3) := 0.00624239
  coeff(4) := 0.02459163
  coeff(5) := 0.03898283
  coeff(6) := 0.04231734
  coeff(7) := 0.02906669
  coeff(8) := 0.00255221
  coeff(8).raw.msb := True
  coeff(9) := 0.04897974
  coeff(9).raw.msb := True
  coeff(10) := 0.10083352
  coeff(10).raw.msb := True
  coeff(11) := 0.1438624
  coeff(11).raw.msb := True
  coeff(12) := 0.16131922
  coeff(12).raw.msb := True
  coeff(13) := 0.13737078
  coeff(13).raw.msb := True
  coeff(14) := 0.0608575
  coeff(14).raw.msb := True
  coeff(15) := 0.07144658
  coeff(16) := 0.25288767
  coeff(17) := 0.46687654
  coeff(18) := 0.68890519
  coeff(19) := 0.89035557
  coeff(20) := 1.04339768
  coeff(21) := 1.12599805
  coeff(22) := 1.12599805
  coeff(23) := 1.04339768
  coeff(24) := 0.89035557
  coeff(25) := 0.68890519
  coeff(26) := 0.46687654
  coeff(27) := 0.25288767
  coeff(28) := 0.07144658
  coeff(29) := 0.0608575
  coeff(29).raw.msb := True
  coeff(30) := 0.13737078
  coeff(30).raw.msb := True
  coeff(31) := 0.16131922
  coeff(31).raw.msb := True
  coeff(32) := 0.1438624
  coeff(32).raw.msb := True
  coeff(33) := 0.10083352
  coeff(33).raw.msb := True
  coeff(34) := 0.04897974
  coeff(34).raw.msb := True
  coeff(35) := 0.00255221
  coeff(35).raw.msb := True
  coeff(36) := 0.02906669
  coeff(37) := 0.04231734
  coeff(38) := 0.03898283
  coeff(39) := 0.02459163
  coeff(40) := 0.00624239
  coeff(41) := 0.00954316
  coeff(41).raw.msb := True
  coeff(42) := 0.01842133
  coeff(42).raw.msb := True
  val fsm = new StateMachine {
    io.result.setIdle()

    val idle: State = new State with EntryPoint {
      whenIsActive {
        for (index <- 1 to (width-1)) {
          signal_hist(index) := signal_hist(index-1)
        }
        when(io.signal.valid) {
          signal_hist(0) := io.signal.payload
          goto(multiply)
        }
      }
    }

    val multiply: State = new State {
      whenIsActive {
          for( i <- 0 to width-1) {
            val res = (signal_hist(i) * coeff(i))
            report(Seq(res.asSInt(), " = ", signal_hist(i).asSInt(), " * ", coeff(i).asSInt()))
            mul(i) := res.saturated
          }
          goto(pushResult)
       }
    }

    val pushResult: State = new State {
      val sum = Reg(outputDataType)
      sum.allowOverride()
      //onEntry(sum := 0.0)
      whenIsActive {
        mul.foreach( a => {sum := a +| sum} )
        report(Seq("sum: ", sum.asSInt()))
        io.result.push(sum)
        goto(idle)
      }
    }
  }
}



case class FirFilter() extends Component {
  val io = new Bundle {
    var result = out SInt(15 bits)
  }
  val filter = new Convolution(AFix.S(2 exp, -12 exp), AFix.S(2 exp, -12 exp),  43)
  val counter = Reg(UInt(6 bits)) init (0)
  val pushSignal = master Flow(AFix.S(2 exp, -12 exp))
  filter.io.signal << pushSignal 
  //pushSignal >> filter.io.signal

  val fsm = new StateMachine {
    val res = Reg(SInt(15 bits))
    io.result := 0
    res := 0
    val pushInput: State = new State with EntryPoint {
      pushSignal.setIdle()
      whenIsActive {
        when (counter === 0) { 
          val tmp = AFix.S(2 exp, -12 exp)
          tmp := 1.0
          pushSignal.push(tmp)
          report(Seq("input: ", tmp.asSInt()))    
        } otherwise { 
          val tmp = AFix.S(2 exp, -12 exp)
          tmp := 0.0
          pushSignal.push(tmp)
          report(Seq("input: ", tmp.asSInt())) 
        }
        goto(pullResult)
      }
    }

    val pullResult: State = new State {
      whenIsActive {
        when (filter.io.result.valid) {
          res := filter.io.result.toReg().asSInt()
          io.result := res
          counter := counter + 1
          goto(pushInput)
        }
      }
    }
  }
}

object MyTopLevelVerilog extends App {
  Config.spinal.generateVerilog(FirFilter())
}

object MyTopLevelVhdl extends App {
  Config.spinal.generateVhdl(FirFilter())
}

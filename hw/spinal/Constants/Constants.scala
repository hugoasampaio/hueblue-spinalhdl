package Constants

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import LimitedFix._
import Constants._

object Constants{
  val IWL: Int = 2
  val FWL: Int = -12
  val cleanMask = B"15'h7fff"
}

case class RRC_FILTER() extends Bundle {
  val rrc_taps = Vec.fill(5)(Reg(LimitedFix(AFix.S(Constants.IWL exp, Constants.FWL exp), 1)))
  rrc_taps.allowOverride()
  rrc_taps(0).getFxp() := 0.01897049
  rrc_taps(0).getFxp().raw.msb := True
  rrc_taps(1).getFxp() := 0.01842133
  rrc_taps(1).getFxp().raw.msb := True
  rrc_taps(2).getFxp() := 0.00954316
  rrc_taps(2).getFxp().raw.msb := True
  rrc_taps(3).getFxp() := 0.00624239
  rrc_taps(4).getFxp() := 0.02459163
  /*
  rrc_taps(5).getFxp() := 0.03898283
  rrc_taps(6).getFxp() := 0.04231734
  rrc_taps(7).getFxp() := 0.02906669
  rrc_taps(8).getFxp() := 0.00255221
  rrc_taps(8).getFxp().raw.msb := True
  rrc_taps(9).getFxp() := 0.04897974
  rrc_taps(9).getFxp().raw.msb := True
  rrc_taps(10).getFxp() := 0.10083352
  rrc_taps(10).getFxp().raw.msb := True
  rrc_taps(11).getFxp() := 0.1438624
  rrc_taps(11).getFxp().raw.msb := True
  rrc_taps(12).getFxp() := 0.16131922
  rrc_taps(12).getFxp().raw.msb := True
  rrc_taps(13).getFxp() := 0.13737078
  rrc_taps(13).getFxp().raw.msb := True
  rrc_taps(14).getFxp() := 0.0608575
  rrc_taps(14).getFxp().raw.msb := True
  rrc_taps(15).getFxp() := 0.07144658
  rrc_taps(16).getFxp() := 0.25288767
  rrc_taps(17).getFxp() := 0.46687654
  rrc_taps(18).getFxp() := 0.68890519
  rrc_taps(19).getFxp() := 0.89035557
  rrc_taps(20).getFxp() := 1.04339768
  rrc_taps(21).getFxp() := 1.12599805
  rrc_taps(22).getFxp() := 1.12599805
  rrc_taps(23).getFxp() := 1.04339768
  rrc_taps(24).getFxp() := 0.89035557
  rrc_taps(25).getFxp() := 0.68890519
  rrc_taps(26).getFxp() := 0.46687654
  rrc_taps(27).getFxp() := 0.25288767
  rrc_taps(28).getFxp() := 0.07144658
  rrc_taps(29).getFxp() := 0.0608575
  rrc_taps(29).getFxp().raw.msb := True
  rrc_taps(30).getFxp() := 0.13737078
  rrc_taps(30).getFxp().raw.msb := True
  rrc_taps(31).getFxp() := 0.16131922
  rrc_taps(31).getFxp().raw.msb := True
  rrc_taps(32).getFxp() := 0.1438624
  rrc_taps(32).getFxp().raw.msb := True
  rrc_taps(33).getFxp() := 0.10083352
  rrc_taps(33).getFxp().raw.msb := True
  rrc_taps(34).getFxp() := 0.04897974
  rrc_taps(34).getFxp().raw.msb := True
  rrc_taps(35).getFxp() := 0.00255221
  rrc_taps(35).getFxp().raw.msb := True
  rrc_taps(36).getFxp() := 0.02906669
  rrc_taps(37).getFxp() := 0.04231734
  rrc_taps(38).getFxp() := 0.03898283
  rrc_taps(39).getFxp() := 0.02459163
  rrc_taps(40).getFxp() := 0.00624239
  rrc_taps(41).getFxp() := 0.00954316
  rrc_taps(41).getFxp().raw.msb := True
  rrc_taps(42).getFxp() := 0.01842133
  rrc_taps(42).getFxp().raw.msb := True
  */
}
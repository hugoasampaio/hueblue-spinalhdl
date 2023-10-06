package LimitedFix

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import Constants._
import LimitedFixMask._
import spinal.core

object LimitedFix {
    def apply(fxp: AFix, id: Int): LimitedFix = {
      val nonMasked = AFix.S(Constants.IWL exp, Constants.FWL exp)
      val masked = AFix.S(Constants.IWL exp, Constants.FWL exp)
      nonMasked := fxp //fix bitWidth from fxp
      masked.raw := nonMasked.raw & B"15'h7fff"
      new LimitedFix(masked)
    }
    
    def apply(fxp: AFix): LimitedFix = {
        new LimitedFix(fxp)
    }
}

case class LimitedFix(fxp: AFix) extends MultiData {

    def assignFromImpl(that: AnyRef,target: AnyRef,kind: AnyRef)
        (implicit loc: spinal.idslplugin.Location): Unit = {
        }
    
    def elements: scala.collection.mutable.ArrayBuffer[(String, spinal.core.Data)] = {
        ArrayBuffer("" -> fxp.raw)
    }
    
    def init(num: BigDecimal): LimitedFix ={
        fxp := num
        this
    }
    
    def getFxp(): AFix = fxp

    def +(that: LimitedFix): LimitedFix = {
        val tmp = AFix.S(Constants.IWL exp, Constants.FWL exp)
        tmp := fxp +| that.getFxp()
        new LimitedFix(tmp.saturated)
    }

    def *(that: LimitedFix): LimitedFix = {
        this * that.getFxp()
    }

    def *(that: AFix): LimitedFix = {
        val tmp = AFix.S(Constants.IWL exp, Constants.FWL exp)
        tmp := fxp * that
        new LimitedFix(tmp.saturated)
    }

    def :=(that: LimitedFix) = {
        val tmp = AFix.S(Constants.IWL exp, Constants.FWL exp)
        tmp := that.getFxp()
        this.fxp := tmp
    }
}


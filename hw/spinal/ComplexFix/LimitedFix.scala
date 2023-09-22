package LimitedFix

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer
import Constants._
import spinal.core

object LimitedFix {
    def apply(fxp: AFix, optimize: Bool): LimitedFix = {
        new LimitedFix(fxp, optimize)
    }
}

class LimitedFix(fxp: AFix, optimize: Bool) extends MultiData {
    var zeroesBitMask = Reg(Bits(fxp.bitWidth bits))
    zeroesBitMask.setAll()
    zeroesBitMask.lsb := False

    
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
        new LimitedFix(tmp.saturated, False)
    }

    def *(that: LimitedFix): LimitedFix = {
        this * that.getFxp()
    }

    def *(that: AFix): LimitedFix = {
        val tmp = AFix.S(Constants.IWL exp, Constants.FWL exp)
        tmp := fxp * that
        new LimitedFix(tmp.saturated, False)
    }

    def :=(that: LimitedFix): LimitedFix = {
        val tmp = AFix.S(Constants.IWL exp, Constants.FWL exp)
        tmp := that.getFxp()
        this.fxp.raw := tmp.raw & zeroesBitMask
        this
    }
}

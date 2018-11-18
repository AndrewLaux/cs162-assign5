package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._

class DeriveSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')
  val f = Chars('f')
  val o = Chars('o')
  val s = Chars('w' -> 'z')

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "matches"

  it should "recognize strings in the language 1" in { 
    val reg = ((f~o)~b)|((f~o)~c)
    val str = "fob"
    val recognize = Derive.matches(reg, str) should equal(true)
  }

  it should "recognize strings in the language 2" in { 
    val reg = (EmptyString|((f~o)~c))
    val str = ""
    val recognize = Derive.matches(reg, str) should equal(true)
  }

  it should "recognize strings in the language 3" in { 
    val reg = (((f~o)~b)|((f~o)~c)).*
    val str = "fobfobfobfoc"
    val recognize = Derive.matches(reg, str) should equal(true)
  }

  it should "recognize strings in the language 4" in { 
    val reg = α.*
    val str = "fobfobfobfoc"
    val recognize = Derive.matches(reg, str) should equal(true)
  }

  it should "recognize strings in the language 5" in { 
    val reg = s
    val str = "x"
    val recognize = Derive.matches(reg, str) should equal(true)
  }

  it should "recognize strings in the language 6" in { 
    val reg = s.*
    val str = "xxyyzzxxyxyxyzzzyz"
    val recognize = Derive.matches(reg, str) should equal(true)
  }

  it should "recognize strings in the language 7" in { 
    val reg = (o<>(4, 6))
    val str = "oooo"
    val recognize = Derive.matches(reg, str) should equal(true)
  }

  it should "recognize strings in the language 8" in { 
    val reg = (!c)
    val str = "f"
    val recognize = Derive.matches(reg, str) should equal(true)
  }

  it should "not recognize strings not in the language 1" in {
    val reg = ((f~o)~b)|((f~o)~c)
    val str = "for"
    val recognize = Derive.matches(reg, str) should equal(false)
  }

  it should "not recognize strings not in the language 2" in {
    val reg = ((f~o)~b)&((f~o)~c)
    val str = "fob"
    val recognize = Derive.matches(reg, str) should equal(false)
  }

  it should "not recognize strings not in the language 3" in {
    val reg = ((f~o)~b)&((f~o)~c)
    val str = ""
    val recognize = Derive.matches(reg, str) should equal(false)
  }

  it should "not recognize strings not in the language 4" in {
    val reg = s
    val str = "v"
    val recognize = Derive.matches(reg, str) should equal(false)
  }

  it should "not recognize strings not in the language 5" in {
    val reg = (o<>(4, 6))
    val str = "ooo"
    val recognize = Derive.matches(reg, str) should equal(false)
  }

  it should "not recognize strings not in the language 6" in {
    val reg = (!b)
    val str = "b"
    val recognize = Derive.matches(reg, str) should equal(false)
  }

  behavior of "eval"

  it should "recognize strings in the language 1" in { 
    val reg = o~c
    val str = "oc"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(true)
  }

  it should "recognize strings in the language 2" in { 
    val reg = (f~(o~c))
    val str = "foc"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(true)
  }

  it should "recognize strings in the language 3" in { 
    val reg = ((f~o)~c)
    val str = "foc"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(true)
  }

  it should "recognize strings in the language 4" in { 
    val reg = ((f~o)~c)|(f~(o~d))
    val str = "fod"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(true)
  }

  it should "recognize strings in the language 5" in { 
    val reg = (f|c).*
    val str = "ffcf"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(true)
  }

  it should "recognize strings in the language 6" in { 
    val reg = (f~c).*
    val str = "fcfc"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(true)
  }

  it should "recognize strings in the language 7" in { 
    val reg = f^5
    val str = "fffff"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(true)
  }

  it should "recognize strings in the language 8" in { 
    val reg = (f|c)|(f~b)|b
    val str = "b"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(true)
  }

  it should "recognize strings in the language 9" in { 
    val reg = ((f|c)|(f~b)).*
    val str = "cfbcfbcccffffccc"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(true)
  }

  it should "recognize strings in the language 10" in { 
    val reg = ((f|c)|(f~b)).*
    val str = ""
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(true)
  }

  it should "not recognize strings not in the language 1" in { 
    val reg = ((f|c)|(f~b)).*
    val str = "cb"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(false)
  }

  it should "not recognize strings not in the language 2" in { 
    val reg = f<>(3,5)
    val str = "ff"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(false)
  }

  it should "not recognize strings not in the language 3" in { 
    val reg = (!b)
    val str = "b"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(false)
  }

  it should "not recognize strings not in the language 4" in { 
    val reg = s
    val str = "v"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(false)
  }

  it should "not recognize strings not in the language 5" in { 
    val reg = s
    val str = "v"
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(false)
  }

  it should "not recognize strings not in the language 6" in {
    val reg = ((f~o)~b)&((f~o)~c)
    val str = ""
    val D = new DerivativeMachine(reg)
    val recognize = D.eval(str) should equal(false)
  }

  behavior of "derive"
  
  it should "compute the correct derivation 1" in {
    DerivativeMachine(∅).derive('f') should equal (∅)
  }

  it should "compute the correct derivation 2" in {
    val r = (f~o)~o
    DerivativeMachine(r).derive('a') should equal (∅)
  }

  it should "compute the correct derivation 3" in {
    val r = (f~o)~o
    DerivativeMachine(r).derive('f') should equal (o~o)
  }

  it should "compute the correct derivation 4" in {
    val r = f | o
    DerivativeMachine(r).derive('f') should equal (ε)
  }

  it should "compute the correct derivation 5" in {
    val r = f.*
    DerivativeMachine(r).derive('f') should equal (r)
  }
}

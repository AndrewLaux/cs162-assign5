package edu.ucsb.cs.cs162.regex.vm

import org.scalatest._
import edu.ucsb.cs.cs162.regex._

class PowersetVmSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._
  import compiler.Compiler
  import parse_tree._


  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "eval"

  // Replace {language 1} with a more descriptive name for what you're testing.
  // Feel free to add more tests, or write many shorter ones.

   
    it should "parse example from piazza" in {

    val username = (Chars('a'->'z').+).capture("username")
    val date = ((Chars('0'->'9') <= 2).capture("day") ~ Chars('/', '-') ~
                  (Chars('0'->'9') <= 2).capture("month") ~ Chars('/', '-') ~
                  (Chars('0'->'9') ^ 4).capture("year")).capture("date")
    val row = username ~ Chars(',') ~ date

    val program = Compiler.compile(row)
    val tree = (new PowersetVm(program)).eval("tom,25/5/2002").get
    val extractor = (new Extractor(tree))

    extractor.extract("username") should equal (List("tom"))
    extractor.extract("date", "day") should equal (List("25"))
    extractor.extract("date", "month") should equal (List("5"))
    extractor.extract("date", "year") should equal (List("2002"))
  }

  it should "parse strings in {language 1}" in { 
    val str = ""

    val program = IndexedSeq(PushEmpty, Accept)

    (new PowersetVm(program)).eval(str).get should equal (EmptyLeaf)
  }

  it should "parse strings in {language a}" in { 
    val str = "a"

    val program = IndexedSeq(
      MatchSet(Chars('a').chars),
      PushChar,
      Accept)

    (new PowersetVm(program)).eval(str).get should equal (CharLeaf('a'))

  }

  it should "parse strings in {language a~a}" in { 
    val str = "aa"

    val program = IndexedSeq(
      MatchSet(Chars('a').chars),
      PushChar,
      MatchSet(Chars('a').chars),
      PushChar,
      PushConcat,
      Accept)

    (new PowersetVm(program)).eval(str).get should equal (ConcatNode(CharLeaf('a'), CharLeaf('a')))

  }

  it should "parse strings in {language a|b}" in { 
    val str = "a"
    val str2 = "b"

    val program = IndexedSeq(
      Fork(1, 5), 
      MatchSet(Chars('a').chars),
      PushChar, 
      PushLeft,
      Jump(4),
      MatchSet(Chars('b').chars),
      PushChar,
      PushRight,
      Accept)
      val vm = new PowersetVm(program)


    vm.eval(str).get should equal (LeftNode(CharLeaf('a')))
    vm.eval(str2).get should equal (RightNode(CharLeaf('b')))

  }

  it should "parse strings in the {language (a.*)}" in {
    val str1= ""
    val str2 = "a"
    val str3 = "aaaa"

    val program = IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(Chars('a').chars),
      PushChar,
      PushStar,
      Jump(-4),
      Accept)

    val vm = new PowersetVm(program)

    new PowersetVm(program).eval(str1).get should equal(StarNode(Seq()))
    new PowersetVm(program).eval(str2).get should equal(StarNode(Seq(CharLeaf('a'))))
    new PowersetVm(program).eval(str3).get should equal(StarNode(
        Seq(
          CharLeaf('a'),
          CharLeaf('a'),
          CharLeaf('a'),
          CharLeaf('a'))))
  
  }

  it should "parse strings in {language (ε|b).*}" in { 
    val str1 = ""
    val str2 = "bbbbb"

    val program = IndexedSeq(
      InitStar,
      CheckProgress,
      Fork(1,10),
      Fork(1, 4), 
      PushEmpty, 
      PushLeft,
      Jump(4),
      MatchSet(Chars('b').chars),
      PushChar,
      PushRight,
      PushStar,
      Jump(-10),
      Accept)

    (new PowersetVm(program)).eval(str1).get should equal (StarNode(Seq()))
    (new PowersetVm(program)).eval(str2).get should equal (StarNode(Seq(RightNode(CharLeaf('b')),
    RightNode(CharLeaf('b')),
    RightNode(CharLeaf('b')),
    RightNode(CharLeaf('b')),
    RightNode(CharLeaf('b')))))

  }

  it should "parse strings in {language (a|b)~(1|2)}" in { 
    val str1 = "a1"
    val str2 = "b1"

    val program = IndexedSeq(
      Fork(1,5), 
      MatchSet(Chars('a').chars), 
      PushChar, 
      PushLeft, 
      Jump(4), 
      MatchSet(Chars('b').chars), 
      PushChar, 
      PushRight, 
      Fork(1,5), 
      MatchSet(Chars('1').chars), 
      PushChar, 
      PushLeft, 
      Jump(4), 
      MatchSet(Chars('2').chars), 
      PushChar, 
      PushRight, 
      PushConcat, 
      Accept)

    (new PowersetVm(program)).eval(str1).get should equal (ConcatNode(LeftNode(CharLeaf('a')), LeftNode(CharLeaf('1'))))
    (new PowersetVm(program)).eval(str2).get should equal (ConcatNode(RightNode(CharLeaf('b')), LeftNode(CharLeaf('1'))))
  }

  
  }

  



  

  

  // more tests...
}

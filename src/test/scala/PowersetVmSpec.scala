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

   
    it should "parse strings in piazza example" in {
    import compiler._
    import parse_tree._ 

    val username = (Chars('a'->'z').+).capture("username")
    val date = ((Chars('0'->'9') <= 2).capture("day") ~ Chars('/', '-') ~
                  (Chars('0'->'9') <= 2).capture("month") ~ Chars('/', '-') ~
                  (Chars('0'->'9') ^ 4).capture("year")).capture("date")
    val row = username ~ Chars(',') ~ date

    val prog = Compiler.compile(row)
    val tree = (new PowersetVm(prog)).eval("tom,25/5/2002").get
    val extractor = (new Extractor(tree))
    extractor.extract("username") should equal (List("tom"))
    extractor.extract("date", "day") should equal (List("25"))
    extractor.extract("date", "month") should equal (List("5"))
    extractor.extract("date", "year") should equal (List("2002"))
    
  }

  it should "parse strings in {language ε}" in { 
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

  it should "parse strings in {language 32bit hex}" in { 

      val str1 = "0x00001111"
      val str2 = "0xABABCCCC"
      val str3 = "0x00F09090"

    val hex = Chars('A' -> 'F') | Chars('0' -> '9')
    val indicator = Chars('0')~Chars('x')
    val prog = IndexedSeq(
      MatchSet(Chars('0').chars), 
      PushChar, 
      MatchSet(Chars('x').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      Accept)


      (new PowersetVm(prog)).eval(str1).get should equal (
      ConcatNode(CharLeaf('0'),
      ConcatNode(CharLeaf('x'),
      ConcatNode(CharLeaf('0'), 
      ConcatNode(CharLeaf('0'),
      ConcatNode(CharLeaf('0'),
      ConcatNode(CharLeaf('0'),
      ConcatNode(CharLeaf('1'),
      ConcatNode(CharLeaf('1'),
      ConcatNode(CharLeaf('1'),CharLeaf('1')))))))))))

      (new PowersetVm(prog)).eval(str2).get should equal (
      ConcatNode(CharLeaf('0'),
      ConcatNode(CharLeaf('x'),
      ConcatNode(CharLeaf('A'), 
      ConcatNode(CharLeaf('B'),
      ConcatNode(CharLeaf('A'),
      ConcatNode(CharLeaf('B'),
      ConcatNode(CharLeaf('C'),
      ConcatNode(CharLeaf('C'),
      ConcatNode(CharLeaf('C'),CharLeaf('C')))))))))))

      (new PowersetVm(prog)).eval(str3).get should equal (
      ConcatNode(CharLeaf('0'),
      ConcatNode(CharLeaf('x'),
      ConcatNode(CharLeaf('0'), 
      ConcatNode(CharLeaf('0'),
      ConcatNode(CharLeaf('F'),
      ConcatNode(CharLeaf('0'),
      ConcatNode(CharLeaf('9'),
      ConcatNode(CharLeaf('0'),
      ConcatNode(CharLeaf('9'),CharLeaf('0')))))))))))


    
  }

  it should "not parse strings not in the language (ε)" in {
    val str = "a"

    val program = IndexedSeq(PushEmpty, Accept)

    Some((new PowersetVm(program)).eval(str)).get should equal (None)

  }

  it should "not parse strings not in the empty language" in {
    val str = "a"

    val program = IndexedSeq(Reject, Accept)

    Some((new PowersetVm(program)).eval(str)).get should equal (None)

  }

  it should "not parse strings not in the {language (a.*)}" in {
    val str1= "b"
    val str2 = "bbbbbbb"

    val program = IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(Chars('a').chars),
      PushChar,
      PushStar,
      Jump(-4),
      Accept)

    val vm = new PowersetVm(program)

    Some(vm.eval(str1)).get should equal(None)
    Some(vm.eval(str2)).get should equal(None)
   
  }

  it should "not parse strings not in {language (a|b)~(1|2)}" in { 
    val str1 = "1a"
    val str2 = "b3"

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

    Some((new PowersetVm(program)).eval(str1)).get should equal (None)
    Some((new PowersetVm(program)).eval(str2)).get should equal (None)
  }

  it should "not parse strings not in {language 32bit hex}" in { 

      val str1 = "0x0000"
      val str2 = "0xABABJJJJ"
      val str3 = "0x^222aaaa"

    val hex = Chars('A' -> 'F') | Chars('0' -> '9')
    val indicator = Chars('0')~Chars('x')
    val prog = IndexedSeq(
      MatchSet(Chars('0').chars), 
      PushChar, 
      MatchSet(Chars('x').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      MatchSet(Chars('A' -> 'F', '0' -> '9').chars), 
      PushChar, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      PushConcat, 
      Accept)


      Some((new PowersetVm(prog)).eval(str1)).get should equal (None)

      Some((new PowersetVm(prog)).eval(str2)).get should equal (None)

      Some((new PowersetVm(prog)).eval(str3)).get should equal (None)


    
  }



  



  

  

}

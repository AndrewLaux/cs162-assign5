package edu.ucsb.cs.cs162.regex.vm.compiler

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.util._


class CompileSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  val f = Chars('f')
  val b = Chars('b')
  val bset = CharSet('b')
  val fset = CharSet('f')

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "compile"

  it should "correctly compile the empty language" in { 
    val reg = ∅
    Compiler.compile(reg) should equal(IndexedSeq(Reject, Accept))
   }

  it should "correctly compile ε" in { 
    Compiler.compile(EmptyString) should equal(IndexedSeq(PushEmpty, Accept))
   }

  it should "correctly compile concatenation" in  { 
    val reg = (f ~ b)
    Compiler.compile(reg) should equal(IndexedSeq(
      MatchSet(fset),
      PushChar,
      MatchSet(bset),
      PushChar,
      PushConcat,
      Accept))
   }

  it should "correctly compile union" in  { 
    //Part of a program posted by anonymous on Piazza 
    val reg = (EmptyString | b)
    Compiler.compile(reg) should equal(IndexedSeq(
      Fork(1, 4), 
      PushEmpty, 
      PushLeft,
      Jump(4),
      MatchSet(bset),
      PushChar,
      PushRight,
      Accept))
   }

  it should "correctly compile kleene star 1" in  { 

    //Nullable reg exp
    val reg = (EmptyString | b).*
    Compiler.compile(reg) should equal(IndexedSeq(
      InitStar,
      CheckProgress,
      Fork(1,10),
      Fork(1, 4), 
      PushEmpty, 
      PushLeft,
      Jump(4),
      MatchSet(bset),
      PushChar,
      PushRight,
      PushStar,
      Jump(-10),
      Accept)) 
  }
  
  it should "correctly compile kleene star 2" in  { 

    //Nullable reg exp
    val reg = b.*
    Compiler.compile(reg) should equal(IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(bset),
      PushChar,
      PushStar,
      Jump(-4),
      Accept)) 
  }

  it should "correctly compile complex regexes 1" in { 
    val reg = ((f~b)~b).*
    Compiler.compile(reg) should equal(IndexedSeq(
      InitStar,
      Fork(1, 11),
      MatchSet(fset),
      PushChar,
      MatchSet(bset),
      PushChar,
      MatchSet(bset),
      PushChar,
      PushConcat,
      PushConcat,
      PushStar,
      Jump(-10),
      Accept
      ))
   }

  it should "correctly compile complex regexes 2" in { 
    val reg = (b^2).+
    Compiler.compile(reg) should equal(IndexedSeq(
      MatchSet(bset),
      PushChar,
      MatchSet(bset),
      PushChar,
      InitStar,
      Fork(1, 8),
      MatchSet(bset),
      PushChar,
      MatchSet(bset),
      PushChar,
      PushConcat,
      PushStar,
      Jump(-7),
      PushConcat,
      PushConcat,
      Accept
      ))

   }

   it should "correctly compile complex regexes 3" in { 
    val reg = b.* ~ f.*
    Compiler.compile(reg) should equal(IndexedSeq(
      InitStar,
      Fork(1,5),
      MatchSet(bset),
      PushChar,
      PushStar,
      Jump(-4),
      InitStar,
      Fork(1,5),
      MatchSet(fset),
      PushChar,
      PushStar,
      Jump(-4),
      PushConcat,
      Accept
      ))

   }

  // more tests...
}

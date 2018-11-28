package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.util._
import edu.ucsb.cs.cs162.range_set._

class DerivativeAnalysisSpec extends FlatSpec with Matchers with Timeout {
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


  
  val cs1 = CharSet('a', 'b')
  val cs2 = CharSet('a', 'b')
  val cs3 = CharSet('a', 'a')
  val cs4 = CharSet('a')
  val cs5 = CharSet('a')
  val cs6 = CharSet('a')

  val set1 = Set(cs1, cs2, cs3)
  val set2 = Set(cs4, cs5, cs6)  

  // The timeout in milliseconds for potentially slow code.
  val timeout = 2000

  // Analyze the given expression subject to a timeout.
  def analyzeWithTimeout(re: Regex) =
    timeoutAfter(timeout) { DerivativeAnalysis.analyze(re) }

  
  // Had to include this here again.
  implicit class PairWise(set: Set[CharSet]) {
    def ∧(other: Set[CharSet]): Set[CharSet] = {
      return set.map(x => other.map(y => y & x)).flatten
    }
  }

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "pairwise intersection"

  it should "compute the correct set of charsets" in {
    (set1 ∧ set2) should equal (set2)
  }

  behavior of "the analysis"

  it should "should always terminate 1" in {
    val charA = Chars('a')

    // Causes a timeout or stack overflow if expression similarity isn't
    // implemented correctly.
    val dfa = analyzeWithTimeout((charA | (charA ~ charA)).*)
  }

  it should "should always terminate 2" in {
    // This test should test that check if normalization and DFA
    // building work well together. If the regexes are not conflated
    // properly, DFA construction would cause a timeout or stack
    // overflow and this test should fail.
    val charA = Chars('a')
    val dfa = analyzeWithTimeout(((charA.+)^2).*)
  }

  // more tests...

  it should "produce a DFA that recognizes the strings in language 1" in {
    val charA = Chars('a')

    val dfa = analyzeWithTimeout(ε | charA)

    dfa.matches("") should equal (true)
    dfa.matches("a") should equal (true)
  }

  it should "produce a DFA tha recognizes the strings in the language 2" in { 
    val reg = (EmptyString|((f~o)~c))
    val str = ""
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (true)
  }

  it should "produce a DFA tha recognizes the strings in the language 3" in { 
    val reg = (((f~o)~b)|((f~o)~c)).*
    val str = "fobfobfobfoc"
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (true)
  }

  it should "produce a DFA tha recognizes the strings in the language 4" in { 
    val reg = α.*
    val str = "fobfobfobfoc"
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (true)
  }

  it should "produce a DFA tha recognizes the strings in the language 5" in { 
    val reg = s
    val str = "x"
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (true)
  }

  it should "produce a DFA tha recognizes the strings in the language 6" in { 
    val reg = s.*
    val str = "xxyyzzxxyxyxyzzzyz"
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (true)
  }

  it should "produce a DFA tha recognizes the strings in the language 7" in { 
    val reg = (o<>(4, 6))
    val str = "oooo"
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (true)
  }

  it should "produce a DFA tha recognizes the strings in the language 8" in { 
    val reg = (!c)
    val str = "f"
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (true)
  }

  // more tests...

  it should "produce a DFA that will not recognize strings not in the language 0" in {
    val charA = Chars('a')

    val dfa = analyzeWithTimeout(ε | charA)

    dfa.matches("b") should equal (false)
    dfa.matches("aa") should equal (false)
  }

  it should "produce a DFA that will not recognize strings not in the language 1" in {
    val reg = ((f~o)~b)|((f~o)~c)
    val str = "for"
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (false)
  }

  it should "produce a DFA that will not recognize strings not in the language 2" in {
    val reg = ((f~o)~b)&((f~o)~c)
    val str = "fob"
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (false)
  }

  it should "produce a DFA that will not recognize strings not in the language 3" in {
    val reg = ((f~o)~b)&((f~o)~c)
    val str = ""
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (false)
  }

  it should "produce a DFA that will not recognize strings not in the language 4" in {
    val reg = s
    val str = "v"
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (false)
  }

  it should "produce a DFA that will not recognize strings not in the language 5" in {
    val reg = (o<>(4, 6))
    val str = "ooo"
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (false)
  }

  it should "produce a DFA that will not recognize strings not in the language 6" in {
    val reg = (!b)
    val str = "b"
    val dfa = analyzeWithTimeout(reg)
    dfa.matches(str) should equal (false)
  }

  

  it should "produce a DFA that has the correct structure 1" in {
    
    val a = Chars('a')
    val b = Chars('b')
    val Σ = CharSet(Char.MinValue → Char.MaxValue)
    val aSet = CharSet('a')
    val bSet = CharSet('b')

    val dfa = DerivativeAnalysis.analyze(a ~ b)

    // Check the initial state, and final states
    dfa.init shouldEqual (a ~ b)
    dfa.fin shouldEqual(Set[Regex](ε))

    // Check if the transition relation is computed for all states
    dfa.delta.keys should contain theSameElementsAs Seq(a ~ b, b, ε, ∅)

    // Check the transition relation
    dfa.delta(a ~ b) should contain theSameElementsAs Seq((!aSet, Regex.∅), (aSet, b))
    dfa.delta(b) should contain theSameElementsAs Seq((!bSet, ∅), (bSet, ε))
    dfa.delta(ε) should contain theSameElementsAs Seq((Σ, ∅))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))

  }

  it should "produce a DFA that has the correct structure 2" in {
    
    val a = Chars('a')
    val Σ = CharSet(Char.MinValue → Char.MaxValue)
    val aSet = CharSet('a')

    val dfa = DerivativeAnalysis.analyze((EmptyString | a).*)

    // Check the initial state, and final states
    dfa.init shouldEqual (EmptyString | a).*
    dfa.fin shouldEqual Set((EmptyString | a).*)

    // Check if the transition relation is computed for all states
    dfa.delta.keys should contain theSameElementsAs Seq((EmptyString | a).*, ∅)

    // Check the transition relation
    dfa.delta(dfa.init) should contain theSameElementsAs Seq((!aSet, Regex.∅), (aSet, (EmptyString | a).*))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  

  }

    it should "produce a DFA that has the correct structure 3" in {
    
    val a = Chars('a')
    val b = Chars('b')
    val Σ = CharSet(Char.MinValue → Char.MaxValue)
    val aSet = CharSet('a')
    val bSet = CharSet('b')
    val abSet = CharSet('a', 'b')

    val dfa = DerivativeAnalysis.analyze(a ~ (a | b))

    // Check the initial state, and final states
    dfa.init shouldEqual (a ~ (a | b))
    dfa.fin shouldEqual(Set[Regex](ε))

    // Check if the transition relation is computed for all states
    dfa.delta.keys should contain theSameElementsAs Seq(a ~ (a | b), (a|b), ε, ∅)

    // Check the transition relation
    dfa.delta(a ~ (a | b)) should contain theSameElementsAs Seq((!aSet, Regex.∅), (aSet,(a | b)))
    dfa.delta((a|b)) should contain theSameElementsAs Seq((abSet, ε), (!abSet, Regex.∅))
    dfa.delta(ε) should contain theSameElementsAs Seq((Σ, ∅))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))

  }

  it should "produce a DFA that has the correct structure 4" in {
    
    val f = Chars('f')
    val o = Chars('o')
    val b = Chars('b')
    val Σ = CharSet(Char.MinValue → Char.MaxValue)
    val fSet = CharSet('f')
    val oSet = CharSet('o')
    val bSet = CharSet('b')

    val dfa = DerivativeAnalysis.analyze((f~o~b).*)

    // Check the initial state, and final states
    dfa.init shouldEqual ((f~o~b).*)
    dfa.fin shouldEqual(Set[Regex]((f~o~b).*))

    // Check if the transition relation is computed for all states
    dfa.delta.keys should contain theSameElementsAs Seq((f~o~b).*, o~b~(f~o~b).*, b~(f~o~b).*, ∅)

    // Check the transition relation
    dfa.delta((f~o~b).*) should contain theSameElementsAs Seq((!fSet, Regex.∅), (fSet, o~b~(f~o~b).*))
    dfa.delta(o~b~(f~o~b).*) should contain theSameElementsAs Seq((!oSet, ∅), (oSet, b~(f~o~b).*))
    dfa.delta(b~(f~o~b).*) should contain theSameElementsAs Seq((!bSet, ∅), (bSet,(f~o~b).* ))
    dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))

  }
  
}

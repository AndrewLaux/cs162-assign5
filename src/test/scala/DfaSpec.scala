package edu.ucsb.cs.cs162.dfa

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex.derivative._

class DfaSpec extends FlatSpec with Matchers with OptionValues {
  import Regex._

  behavior of "Dfa.getString"
  val u = Chars('u')
  val b = Chars('b')
  val set1 = Chars('1'->'3')
  val set2 = Chars('2'->'3')
  val set3 = Chars('4')

  it should "return None if the DFA's language is empty 1" in {
    val δ = Map(∅ → Seq(!CharSet() → ∅))
    val dfa = Dfa(δ, ∅, Set.empty)
    dfa.getString shouldEqual None
  }

  it should "return None if the DFA's language is empty 2" in {
    val dfa = DerivativeAnalysis.analyze(Intersect(set1, set3))
    dfa.getString shouldEqual None
  }

  it should "return None if the DFA's language is empty 3" in {
    val δ: Transitions[Regex] = Map((u~b~u~b) → Seq(CharSet('u') → (b~u~b)), (b~u~b) → Seq(CharSet('b') → (u~b)), (u~b) -> Seq(CharSet('u') -> b), b -> Seq(CharSet('b')->(ε)))
    val dfa = Dfa(δ, ε, Set.empty)
    dfa.getString shouldEqual None
  }

  

  // more tests...

  it should "return a string that the DFA recognizes if the DFA's language is not empty 1" in {
    val δ: Transitions[Regex] = Map(ε → Seq(!CharSet() → ∅), ∅ → Seq(!CharSet() → ∅))
    val dfa = Dfa(δ, ε, Set[Regex](ε))
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 2" in {
    val δ: Transitions[Regex] = Map((u~b~u~b) → Seq(CharSet('u') → (b~u~b)), (b~u~b) → Seq(CharSet('b') → (u~b)), (u~b) -> Seq(CharSet('u') -> b), b -> Seq(CharSet('b')->(ε)))
    val dfa = Dfa(δ, ε, Set[Regex](ε))
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 3" in {
    val dfa = DerivativeAnalysis.analyze(u.*)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 4" in {
    val dfa = DerivativeAnalysis.analyze(α.+)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  // more tests...
}

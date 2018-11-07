// Provides a derivative-based static analysis of regular expressions that
// yields a DFA describing the language recognized by an expression.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.dfa._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._

object DerivativeAnalysis {
  import Derive._
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Statically analyzes 're' using derivatives in order to compute the DFA of
  // the language recognized by 're'. The resulting DFA has an explicit error
  // state and is approximately minimal.
  def analyze(re: Regex): Dfa[Regex] = {
    val statesAndTransitions = computeDfa(Set(re), Set[Regex](), Map[Regex, Seq[(CharSet, Regex)]]())
    Dfa(statesAndTransitions._2, re, statesAndTransitions._1.filter(_.nullable == ε))
  }

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  
  implicit class PairWise(set: Set[CharSet]) {
    def ∧(other: Set[CharSet]): Set[CharSet] = {
      return set.map(x => other.map(y => y & x)).flatten
    }
  }

  //Define C for use in computeNexr
  def cFunc(s: Regex): Set[CharSet] = s match{
      case `∅` => return Set(α.chars)
      case `ε` => return Set(α.chars)
      case Chars(s) => return Set(s, α.chars & !s)
      case KleeneStar(r) => return cFunc(r)
      case Complement(r) => return cFunc(r)
      case Union(r, s) => return cFunc(r) ∧ cFunc(s)
      case Intersect(r, s) => return cFunc(r) ∧ cFunc(s)
      case Concatenate(r, s) if (r.nullable == `∅`) => return cFunc(r)
      case Concatenate(r, s) => return cFunc(r) ∧ cFunc(s)
    }

  // Compute the transitions and set of reachable states (i.e., Regexes) for all
  // Regexes in 'todo'.
  @annotation.tailrec
  private def computeDfa(todo: Set[Regex], visitedStates: Set[Regex], transitions: Transitions[Regex]) : (Set[Regex], Transitions[Regex]) = {

    //No more transitions to examine.
    if (todo.isEmpty) {
      return  (visitedStates, transitions)
    }

    //Continue examinig states.
    else {
      val currentdeltas = computeNext(todo.head)
      return computeDfa(currentdeltas._1.filter(!visitedStates.contains(_)) ++ todo.drop(1), visitedStates + todo.head, transitions ++ currentdeltas._2)
    }
  }

  // Compute the transitions and destination states from the given regex.
  private def computeNext(state: Regex): (Set[Regex], Transitions[Regex]) = {

    //Get partitions of state.
    val partitions = cFunc(state)

    //Map each partition to a possible transition state.
    val deltas = partitions.collect{ case x if !x.isEmpty => (x, DerivativeMachine(state).derive(x.minElement.get))}

    //Get all states visitable
    val q = deltas.map( x => x._2)

    //Return results
    return (q , Map(state -> deltas.toSeq))
    

  }


}

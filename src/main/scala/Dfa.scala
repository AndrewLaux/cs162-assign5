// Provides the Dfa class for deterministic finite automata.

package edu.ucsb.cs.cs162.dfa

import edu.ucsb.cs.cs162.range_set._

object `package` {
  type Transitions[State] = Map[State, Seq[(CharSet, State)]]
}

// The DFA. 'delta' is the state transition map; 'init' is the initial state;
// 'fin' is the set of final states. The DFA is assumed to have an explicit
// error state and the transitions are assumed to cover the complete range of
// possible characters.
case class Dfa[State](delta: Transitions[State], init: State, fin: Set[State]) {
  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Returns true iff the given string is recognized by the DFA.
  def matches(str: String): Boolean =
    fin.contains(trace(init, str))

  // Returns a string that causes an arbitrary but non-looping path from the
  // init state to a final state, if such a path exists.
  def getString: Option[String] = {
  if (fin.isEmpty) return None
  val path = find(init, Set[State](), fin.head).get
  return Option((path.zip(path.tail)).foldLeft(""){ (z, i: (State, State)) =>  z + delta(i._1).find(_._2 == i._2).get._1.minElement.get.toString })
  } 

  //Depth first traversal of DFA searching for target:
  def find(current: State, seen: Set[State], target: State ): Option[Seq[State]] = {
    if (current == target) return Some(Seq(current))
    else delta(current).foreach{ 
      next => {
        if(!seen.contains(next._2)){
          val far = find(next._2, seen + next._2, target)
          if(far.isDefined) return Some(Seq(current) ++ far.get)
        }
      }
    }
    return None
  }

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Compute the state reached by tracing the given string through the DFA.
  @annotation.tailrec
  private def trace(state: State, str: String): State =
    if (str.isEmpty) state
    else delta(state).find(_._1.contains(str.head)) match {
      case Some((_, next)) => trace(next, str.tail)
      case None => {
        assert(false, "should be unreachable")
        state
      }
    }
}

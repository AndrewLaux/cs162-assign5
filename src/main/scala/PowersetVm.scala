package edu.ucsb.cs.cs162.regex.vm

import edu.ucsb.cs.cs162.regex.parse_tree._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._

// A virtual machine that uses Thompson's powerset strategy to implement a
// non-backtracking algorithm for regular expression matching.
class PowersetVm(program: Program) extends VirtualMachine(program) {
  override def eval(str: String): Option[ParseTree] = {
    // Algorithm:
    // 1. compute initial set of threads (the ε-closure of the nfa start state)
    // 2. if the input string is empty go to step 7
    // 3. run the threads until they reach a match or accept instruction
    // 4. compact them to enforce at most one thread per program counter
    // 5. execute the surviving threads one step (i.e., the match or accept instruction)
    // 6. go to step 2
    // 7. compact the final set of threads
    // 8. if there is a surviving thread at an accept instruction, then that
    //    thread's 'parse' contains the final answer; otherwise there is no answer

    // Execute all given threads until they reach either a MatchSet or an Accept
    // instruction; returns the resulting set of Threads.
    //@annotation.tailrec
    def runUntilMatchOrAccept(thread: Thread, todo: Set[Thread],
      result: Set[Thread]): Set[Thread] = program(thread.pc) match {

        case `Accept` => if(!todo.isEmpty) return runUntilMatchOrAccept(todo.head, todo.tail, result + thread) else return result + thread 
          

        case `Reject` => if(!todo.isEmpty) return runUntilMatchOrAccept(todo.head, todo.tail, result) else return result
      

        case `CheckProgress` => {

          //Encounter a 'Check' form before.
           if (thread.progress contains(thread.pc)) {
             if (todo.isEmpty) return result
             else return runUntilMatchOrAccept(todo.head, todo.tail, result)
           }

          //Encounter a new 'Check'
          else {
            return runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress + thread.pc, thread.priority, thread.parse), todo, result)
          }
        }
          

        case MatchSet(chars) => if(!todo.isEmpty) return runUntilMatchOrAccept(todo.head, todo.tail, result + new Thread(thread.pc, Set(), thread.priority, thread.parse)) else result + new Thread(thread.pc, Set(), thread.priority, thread.parse) 
          

        case Jump(offset) => return runUntilMatchOrAccept(new Thread(thread.pc + offset, thread.progress, thread.priority, thread.parse), todo, result)
        

        case Fork(offset1, offset2) => return runUntilMatchOrAccept(new Thread(thread.pc + offset1, thread.progress, thread.priority, thread.parse), todo + new Thread(thread.pc + offset2, thread.progress, thread.priority, thread.parse), result )
          

        case `PushEmpty` => return runUntilMatchOrAccept( new Thread(thread.pc + 1, thread.progress, thread.priority,  EmptyLeaf +: thread.parse ), todo, result)
        

        case `PushConcat` => return runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority, ConcatNode(thread.parse.tail.head, thread.parse.head) +: thread.parse.tail.tail), todo, result)
          

        case `PushLeft` => return runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, "l", LeftNode(thread.parse.head) +: thread.parse.tail), todo, result)
          

        case `PushRight` => return runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, "r", RightNode(thread.parse.head) +: thread.parse.tail), todo, result)
          

        case `InitStar` => return runUntilMatchOrAccept( new Thread(thread.pc + 1, thread.progress, thread.priority, StarNode(Seq()) +: thread.parse ), todo, result)
          

        case `PushStar` => {
        val body = thread.parse.head
        val star = thread.parse.tail.head
        val rest = thread.parse.tail.tail

          star match {
            case StarNode(seq) => return runUntilMatchOrAccept(new Thread(thread.pc + 1, thread.progress, thread.priority, StarNode(body +: seq) +: rest ), todo, result)
            case _ => {
              assert(false, "should be unreachable")
              Set()
            }
          }
        }
          
        case PushCapture(name) => return runUntilMatchOrAccept(new Thread(thread.pc +1, thread.progress, thread.priority, CaptureNode(name, thread.parse.head) +: thread.parse.tail), todo, result)

      }   

    // Remove any threads s.t. there exists another thread at the same program
    // point with a smaller Priority.
    def compact(threads: Set[Thread]): Set[Thread] = {
      return threads.groupBy(_.pc).map{ case(k,s)  => (k, s.minBy(_.priority))}.values.toSet
    }

    // Return the result of matching the current string position on all the
    // given threads.
    val matchStringPosition: (Set[Thread], Char) => Set[Thread] = (threads, char) => {
        threads.foldLeft(Set[Thread]()) { (accum, thread) => {
            (program(thread.pc)) match {
               case MatchSet(chars) if chars.contains(char) =>
                 program(thread.pc + 1) match {
                   
                   case `PushChar` => accum + new Thread(thread.pc + 2, thread.progress, thread.priority, CharLeaf(char) +: thread.parse )

                   case _ => {
                     assert(false, "Should be unreachable")
                     accum
                   }
                 }
              case _ => accum
            }
          }
        }
      }
      

    //Powerset algorithm
    val start = Thread(0, Set[Int](), "", Seq[ParseTree]())

    val traversals = str.foldLeft(Set[Thread](start)) {
      (acc, ch) => acc.isEmpty match {
        case false => matchStringPosition(runUntilMatchOrAccept(acc.head, acc.tail, Set[Thread]()), ch)
        case true => acc
      }
    }

    //No traversals.
    if (traversals.isEmpty) return None

    //Possible traversals.
    else {
      val accepting = compact(runUntilMatchOrAccept(traversals.head, traversals.tail, Set[Thread]())).filter(thread => program(thread.pc) == Accept)

      //There are no accepting threads.
      if (accepting.isEmpty) return None

      //We have accepting threads.
      else return Some(accepting.head.parse.head)
    }
  }

  // A thread of execution for the VM, where 'pc' is the program counter,
  // 'progress' is the set of encountered CheckProgress instructions, 'priority'
  // is the thread priority (lower is better), 'parse' is the current parsing
  // stack. We don't need a separate string position per thread because all
  // executing threads will, by construction, always be at the same string
  // position.
  private case class Thread(pc: Int, progress: Set[Int], priority: String,
    parse: Seq[ParseTree])
}

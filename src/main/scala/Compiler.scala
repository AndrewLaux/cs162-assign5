// This is the compiler that translates regex abstract syntax trees into regular
// expression matching virtual machine programs.

package edu.ucsb.cs.cs162.regex.vm.compiler

import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm._

object Compiler {
  // Return a virtual machine program that implements the given regex.
  def compile(re: Regex): Program = return parse(re) :+ Accept

  def parse(re: Regex): Program = {

    //Match regex operation
    re match{

      //Concatenation
      case Concatenate(r, s) => {
        return (parse(r) ++ parse(s)) ++ IndexedSeq(PushConcat)
      }

      //Union
      case Union(r, s) => {
        val left = parse(r)
        val right = parse(s)
        return (((((IndexedSeq(Fork(1, left.length + 3)) 
        ++ left) :+ PushLeft) :+ Jump(right.length + 2)) 
        ++ right) :+ PushRight)
      }

      //KleeneStar
      case KleeneStar(r) => {
        val rprg = parse(r)
        if(r.nullable == EmptyString)
          return ((((IndexedSeq(InitStar) :+ CheckProgress) :+ Fork(1, rprg.length + 3))
          ++ rprg) :+ PushStar) :+ Jump(-(rprg.length +3))
        else
          return ((((IndexedSeq(InitStar) :+ Fork(1, rprg.length + 3)) ++ rprg) :+ PushStar) 
          :+ Jump(-(rprg.length + 2)))      
      }
      
      //EmptyString
      case EmptyString => return IndexedSeq(PushEmpty)
      //Empty lang
      case Chars(a) if(a.isEmpty) => IndexedSeq(Reject)
      //Charset
      case Chars(a) => {
        return IndexedSeq(MatchSet(a), PushChar)
      }
    }
  }
}

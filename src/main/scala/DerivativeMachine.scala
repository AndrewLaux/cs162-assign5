// A virtual machine implementation of derivative-based matching.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.regex._

object `package` {
  // Programs for the DerivativeMachine.
  type Program = Seq[Instruction]

  // Pretty-print derivative virtual machine programs.
  def programToString(prog: Program): String = {
    val strs = for (inst <- prog) yield inst match {
      case `PushDerive` => "derive"
      case `PushConcatenate` => "concatenate"
      case `PushUnion` => "union"
      case `PushComplement` => "complement"
      case `PushIntersect` => "intersect"
      case `PushNullable` => "nullable"
      case PushRe(re) => "push " + re.toString
    }
    strs.mkString("\n")
  }
}

// Instructions for the virtual machine.
//
// - Derive: pop the top of the operand stack, compute its derivative w.r.t. the
//   machine's given char, then push the result back on the operand stack.
// - PushConcatentate: pop the top two elements of the operand stack and push
//   their concatenation back on.
// - PushUnion: pop the top two elements of the operand stack and push their
//   union back on.
// - PushComplement: pop the top of the operand stack, take its complement, and
//   push the result back on.
// - PushIntersect: pop the top two elements of the operand stack and push
//   their intersection back on.
// - PushNullable: pop the top of the operand stack, compute its nullability,
//   and push the result back on the operand stack.
// - PushRe(re): push re onto the top of the operand stack.
sealed abstract class Instruction
case object PushDerive extends Instruction
case object PushConcatenate extends Instruction
case object PushUnion extends Instruction
case object PushComplement extends Instruction
case object PushIntersect extends Instruction
case object PushNullable extends Instruction
case class PushRe(re: Regex) extends Instruction

object DerivativeMachine {
  def apply(re: Regex) = new DerivativeMachine(re)
}

class DerivativeMachine(re: Regex) {
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Returns true iff 'str' is recognized by 're'.
  def eval(str: String): Boolean = {
    val instruct = Seq(PushDerive)
    return str.foldLeft(re)((currentRe, char) => run(Seq(currentRe), instruct, char)).nullable == ε
  }

  // Returns the derivative of 're' w.r.t. 'char'.
  def derive(char: Char): Regex = run(Seq(re), Seq(PushDerive), char)

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------


  // Derives a regular expression from the top of 'operands' w.r.t. 'char'.
  // @annotation.tailrec
  private def run(operands: Seq[Regex], program: Program, char: Char): Regex = {
    if (program.isEmpty) {
      assert(operands.size == 1)
      operands.head
    }
    else {
      val instruct = program.last
      val newprog = program.init

      //Interpret instructions
      instruct match{

        //Push Regex
        case PushRe(x) => {
          return run(operands :+ x, newprog, char)
        }

        //Concatenate
        case PushConcatenate => {
          assert(operands.size > 1)
          val op2 = operands.last
          val temp = operands.init
          val op1 = temp.last
          val finalops = temp.init
          val finalprog = newprog :+ PushRe(op1~op2)
          return run(finalops, finalprog, char)
        }

        //Union
        case PushUnion => {
          assert(operands.size > 1)
          val op2 = operands.last
          val temp= operands.init
          val op1 = temp.last
          val finalops = temp.init
          val finalprog = newprog :+ PushRe(op1|op2)
          return run(finalops, finalprog, char)
        }

        //Intersect
        case PushIntersect => {
          val op2 = operands.last
          val temp= operands.init
          val op1 = temp.last
          val finalops = temp.init
          val finalprog = newprog :+ PushRe(op1&op2)
          return run(finalops, finalprog, char)
        }

        //Compliment
        case PushComplement => {
          val op1 =operands.last
          val finalops = operands.init
          val finalprog= newprog :+ PushRe(!op1)
          return run(finalops, finalprog, char)
        }

        //Nullable
        case PushNullable => {
          val op1 =operands.last
          val finalops = operands.init
          val finalprog= newprog :+ PushRe(op1.nullable)
          return run(finalops, finalprog, char)
        }

        //Derive w.r.t. char
        case PushDerive => {
          val op1 = operands.last
          val temp = operands.init

          //Perform one step of derivation on op1
          op1 match {

            //Union w.r.t. char
            case Union(re1, re2) => {
              val seq = Seq( PushUnion,
              PushDerive,
              PushRe(re2),
              PushDerive,
              PushRe(re1)
              )
              return run(temp, newprog ++ seq, char)
            }

            //Concatenation
            case Concatenate(a, b) => {
              val seq = Seq(
                PushUnion,
                PushConcatenate,
                PushDerive,
                PushRe(b),
                PushNullable,
                PushRe(a),
                PushConcatenate,
                PushRe(b),
                PushDerive,
                PushRe(a)
              )
              return run(temp, newprog ++ seq, char)
            }

            //Intersection
            case Intersect(a, b) => {
              val seq = Seq(
                PushIntersect,
                PushDerive,
                PushRe(b),
                PushDerive,
                PushRe(a)
              )
              return run(temp, newprog ++ seq, char)
            }

            //KleeneStar w.r.t. char
            case KleeneStar(r) => {
              val seq = Seq(
                PushConcatenate,
                PushRe(op1),
                PushDerive,
                PushRe(r)
              )
              return run(temp, newprog ++ seq, char)
            }

            //Compliment
            case Complement(r) => {
              val seq = Seq(
                PushComplement,
                PushDerive,
                PushRe(r)
              )
              return run(temp, newprog ++ seq, char)
            }

            //Empty lang
            case `∅` => {
              val finalops = temp :+ Chars()
              return run(finalops, newprog, char)
            }
            //Singleton Character w.r.t. char
            case Chars(c) if(c.contains(char)) => {
              val finalops = temp :+ EmptyString
              return run(finalops, newprog, char)
            }

            //Anyother charset where char is not in the set
            case Chars(c) if(!c.contains(char)) => {
              val finalops = temp :+ Chars()
              return run(finalops, newprog, char)
            }

            //The Emptystring
            case EmptyString =>  {
              val finalops = temp :+ Chars()
              return run(finalops, newprog, char)
            }
          }
        }
      }
    }
  }
}

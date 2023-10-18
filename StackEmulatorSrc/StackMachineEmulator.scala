package edu.colorado.csci3155.project1
import scala.annotation.tailrec



sealed trait StackMachineInstruction
/*-- Complete the byte code instructions as specified in the documentation --*/
case class LoadEnv(s: String) extends StackMachineInstruction
case class  StoreEnv(s: String) extends StackMachineInstruction
case object PopEnv extends StackMachineInstruction

case class PushNumI(f: Double) extends StackMachineInstruction
case class PushBoolI(b: Boolean) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object GeqI extends StackMachineInstruction
case object EqI extends StackMachineInstruction 
case object NotI extends StackMachineInstruction
case object PopI extends StackMachineInstruction

case class CSkipI(numToSkip: Int) extends StackMachineInstruction
case class SkipI(numToSkip: Int) extends StackMachineInstruction

object StackMachineEmulator {

    /*-- An environment stack is a list of tuples containing strings and values --*/
    type RuntimeStack = List[(String, Value)]
    /*-- An operand stack is a list of values --*/
    type OpStack = List[Value]

    /* Function emulateSingleInstruction
        Given a list of values to represent a operand stack
              a list of tuples (string, value) to represent runtime stack
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified runtime that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: OpStack,
                                 env: RuntimeStack,
                                 ins: StackMachineInstruction): (OpStack, RuntimeStack) = {
        ins match {
            /*TODO:  Your code here must handle each instruction type and 
                     execute the appropriate instructions to modify the 
                     runtime/operand stacks as specified */
            case PushNumI(f) => (Num(f) :: stack, env)
            case PushBoolI(b) => (Bool(b) :: stack, env)


            
            case PopI => 
                stack match {
                    case Nil => throw new RuntimeException("Empty Operand Stack")
                    case headstackOP :: tailstackOP => (tailstackOP,env)
                }
            
            case AddI => 
                stack match {
                    case Num(x) :: Num(y) :: tailstackOP => (Num(x+y) :: tailstackOP,env)
                    case _ => throw new RuntimeException("Empty Operand Stack")
                }
            case SubI => 
                stack match {
                    case Num(v1) :: Num(v2) :: tailstackOP => (Num(v2-v1) :: tailstackOP,env)
                    case _ => throw new RuntimeException("Empty Operand Stack")
                }
            case MultI => 
                stack match {
                    case Num(x) :: Num(y) :: tailstackOP => (Num(x*y) :: tailstackOP,env)
                    case _ => throw new RuntimeException("Empty Operand Stack")
                }  
            case DivI => 
                stack match {
                    case Num(v1) :: Num(v2) :: tailstackOP => (Num(v2/v1) :: tailstackOP,env)
                    case _ => throw new RuntimeException("Empty Operand Stack")
                }
            case LogI => 
                stack match {
                    case Num(x) :: tailstackOP => 
                        if (x >= 0) { (Num(math.log(x)) :: tailstackOP,env) }
                        else { throw new RuntimeException("Negative X Value") }
                    case _ => throw new RuntimeException("Empty Operand Stack")
                }
            case ExpI => 
                stack match {
                    case Num(x) :: tailstackOP => (Num(math.exp(x)) :: tailstackOP,env)
                    case _ => throw new RuntimeException("Lazy Error1")
                }
            case SinI => 
                stack match {
                    case Num(x) :: tailstackOP => (Num(math.sin(x)) :: tailstackOP,env)
                    case _ => throw new RuntimeException("Lazy Error2")
                }
            case CosI => 
                stack match {
                    case Num(x) :: tailstackOP => (Num(math.cos(x)) :: tailstackOP,env)
                    case _ => throw new RuntimeException("Lazy Error3")
                }
            case GeqI => 
                stack match {
                    case Num(v1) :: Num(v2) :: tailstackOP => (Bool(v2 >= v1) :: tailstackOP,env)
                    case _ => throw new RuntimeException("Lazy Error4")
                }
            case EqI => 
                stack match {
                    case v1 :: v2 :: tailstackOP => (Bool(v1 == v2) :: tailstackOP,env)
                    case _ => throw new RuntimeException("Lazy Error5")
                }
            case NotI => 
                stack match {
                    case Bool(b) :: tailstackOP => (Bool(!b) :: tailstackOP, env)
                    case _ => throw new RuntimeException("Lazy Error6")
                }

            

            

            case LoadEnv(id) => 
                stack match {
                    case headstackOP :: tailstackOP => (tailstackOP, (id,headstackOP) :: env)
                    case Nil => throw new RuntimeException("Empty Operand Stack")
                }          

            // weird one here
            case StoreEnv(identifier) =>
                // grab first case with matching id
                val tupleVal = env.collectFirst {
                    case (id, v) if id == identifier => v
                }
                // see of Some(value) exist, if so append to stack
                // else said None say 
                tupleVal match {
                    case Some(value) =>
                    (value :: stack, env)
                    case None =>
                    throw new RuntimeException("Identifier Not Found")
                }




                


            case PopEnv =>
                env match {
                    case Nil => throw new RuntimeException("Empty Runtime Stack")
                    case headenv :: tailenv => (stack,tailenv)
                }

            case CSkipI(n) =>
                stack match {
                    case Bool(false) :: tailstackOP => 
                        if (tailstackOP.length >= n + 1) {
                            (tailstackOP.drop(n),env)
                        }
                        else{
                            throw new RuntimeException("Not Enough Space: Operand Stack")
                        }
                    case Bool(true) :: tailstackOP => (tailstackOP,env)
                    case _ => throw new RuntimeException("CSkipI failed")
                }
            case SkipI(n) =>
                if (stack.length >= n + 1) { (stack.drop(n),env) }
                else { throw new RuntimeException("SkipI failed") }

            
            
            




            case _ => throw new RuntimeException(s"Unknown instruction type: $ins ")
        }
        
    }

    // initial idea, ignore lazy error
    // case StoreEnv(ident) =>
    //     env.headOption match {
    //         case Some((id, value)) if id == ident =>
    //         (value :: stack, env)
    //         case _ => throw new RuntimeException("Lazy Error5")
    //     }




    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Return the final runtimeStack and the top element of the opStack
     */
    @tailrec
    def emulateStackMachine(instructionList: List[StackMachineInstruction], 
                            opStack: OpStack=Nil, 
                            runtimeStack: RuntimeStack=Nil): (Value, RuntimeStack) =
        {
            /*-- Are we out of instructions to execute --*/
            if (instructionList.isEmpty){
                /*-- output top elt. of operand stack and the runtime stack --*/
                (opStack.head, runtimeStack)
            } else {
                /*- What is the instruction on top -*/
                val ins = instructionList.head
                ins match {
                    /*-- Conditional skip instruction --*/
                    case CSkipI(n) => {
                        /* get the top element in operand stack */
                        val topElt = opStack.head 
                        val restOpStack = opStack.tail 
                        val b = topElt.getBooleanValue /* the top element better be a boolean */
                        if (!b) {
                            /*-- drop the next n instructions --*/
                            val restOfInstructions = instructionList.drop(n+1)
                            emulateStackMachine(restOfInstructions, restOpStack, runtimeStack)
                        } else {
                            /*-- else just drop this instruction --*/
                            emulateStackMachine(instructionList.tail, restOpStack, runtimeStack)
                        }
                    }
                    case SkipI(n) => {
                        /* -- drop this instruction and next n -- continue --*/
                        emulateStackMachine(instructionList.drop(n+1), opStack, runtimeStack)
                    }

                    case _ => {
                        /*- Otherwise, just call emulateSingleInstruction -*/
                        val (newOpStack: OpStack, newRuntime:RuntimeStack) = emulateSingleInstruction(opStack, runtimeStack, ins)
                        emulateStackMachine(instructionList.tail, newOpStack, newRuntime)
                    }
                }
            }
        }
}
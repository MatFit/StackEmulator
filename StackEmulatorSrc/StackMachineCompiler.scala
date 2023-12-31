package edu.colorado.csci3155.project1

object StackMachineCompiler {


    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        /* Begin Solution */
        e match {
            /* TODO: Your code here must handle the cases for Expr (see Expr.scala) */
            case BoolConst(b) => List(PushBoolI(b))
            case Ident(str) => List(StoreEnv(str))
            case Const(f) => List(PushNumI(f))
            
            case Plus(e1,e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode (e2) ++ List(AddI)
            case Minus(e1,e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode (e2) ++ List(SubI)
            case Mult(e1,e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode (e2) ++ List(MultI)
            case Div(e1,e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode (e2) ++ List(DivI)
            case Exp(e) => compileToStackMachineCode(e) ++ List(ExpI)
            case Log(e) => compileToStackMachineCode(e) ++ List(LogI)
            case Sine(e) => compileToStackMachineCode(e) ++ List(SinI)
            case Cosine(e) => compileToStackMachineCode(e) ++ List(CosI)
            case Geq(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(GeqI)
            case Eq(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(EqI)
            case Not(e) => compileToStackMachineCode(e) ++ List(NotI)

            case Let(ident, e1, e2) => compileToStackMachineCode(e1) ++ List(LoadEnv(ident)) ++ compileToStackMachineCode(e2) ++ List(PopEnv)
            case IfThenElse(cond, e1, e2) => compileToStackMachineCode(cond) ++ List(CSkipI(compileToStackMachineCode(e1).length + 1)) ++ compileToStackMachineCode(e1) ++ List(SkipI(compileToStackMachineCode(e2).length)) ++ compileToStackMachineCode(e2)

            case And(e1, e2) => compileToStackMachineCode(e1) ++ List(CSkipI(compileToStackMachineCode(e2).length + 1)) ++ compileToStackMachineCode(e2) ++ List(SkipI(1)) ++ List(PushBoolI(false))
            case Or(e1, e2) => compileToStackMachineCode(e1) ++ List(CSkipI(compileToStackMachineCode(e1).length - 1)) ++ List(PushBoolI(true)) ++ List(SkipI(compileToStackMachineCode(e2).length)) ++ compileToStackMachineCode(e2)
            
            case _ => throw new IllegalArgumentException(s"I do not handle $e")
        }
        /* End Solution */
    } 
}

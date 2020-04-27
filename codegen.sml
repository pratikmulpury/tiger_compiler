signature CODEGEN = 
sig
    structure Frame : FRAME
    val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen: CODEGEN = 
struct
structure Frame = MipsFrame
structure T = Tree

    fun relopToMips(relop) =
        case relop of 
        T.GT => "bgt"
        | T.GE => "bge"
        | T.EQ => "beq"
        | T.NE => "bne"
        | T.LE => "ble"
        | T.LT => "blt"
        | _ => "shouldnthappen"

    fun binopToMips(relop) =
        case relop of 
        T.PLUS => "add"
        | T.MINUS => "sub"
        | T.MUL => "mul"
        | T.DIV => "div"
        | T.OR => "or"
        | T.AND => "and"
        | _ => "shouldnthappen"
    fun int(i) =  
        if i< 0
        then "-" ^ int(~i)
        else Int.toString(i)

    fun codegen(frame)(stm: T.stm) : Assem.instr list =
        let
            val ilist = ref (nil: Assem.instr list)
            fun emit x= ilist := x :: !ilist
            fun result(gen) = let val t = Temp.newtemp() in gen t; t end
            fun munchStm(T.SEQ(a,b)) = (munchStm(a); munchStm(b))
                | munchStm(T.EXP(e1)) = (munchExp(e1); ())
                | munchStm(T.JUMP(T.NAME(lab),labs)) = 
                    emit(Assem.OPER{
                        assem="j " ^ Symbol.name(lab) ^ " \n", 
                        src=[], 
                        dst=[], 
                        jump=SOME(labs)
                    })
                | munchStm(T.JUMP(e,labs)) = 
                    emit(Assem.OPER{
                        assem="jr `s0\n", 
                        src=[munchExp(e)], 
                        dst=[], 
                        jump=SOME(labs)
                    })
			    | munchStm (T.CJUMP(relop, e1, e2, t, f)) = 
                    emit (Assem.OPER{
                        assem=relopToMips(relop) ^ " `s0, `s1, " ^ Symbol.name(t) ^ " \n",
                        src=[munchExp(e1), munchExp(e2)], 
                        dst=[], 
                        jump=SOME([t, f])
                    })   
                | munchStm(T.LABEL lab) = 
                    emit(Assem.LABEL{
                        assem=Symbol.name(lab) ^ ":\n",
                        lab=lab
                    })
                | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i)),e2)) =
                    emit(Assem.OPER {
                        assem="sw `s1, " ^ (int i) ^ "(`s0) \n",
                        src=[munchExp(e1), munchExp(e2)], 
                        dst=[], 
                        jump=NONE
                    })
                | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)),e2)) =
                    emit(Assem.OPER {
                        assem="sw `s1, " ^ (int i) ^ "(`s0) \n",
                        src=[munchExp(e1), munchExp(e2)], 
                        dst=[], 
                        jump=NONE
                    })                  
                | munchStm(T.MOVE(T.MEM(T.CONST i),e2)) =
                    emit(Assem.OPER{
                        assem="sw `s0, " ^ int i ^ "($zero)\n",
                        src=[munchExp(e2)],
                        dst=[], 
                        jump=NONE
                    })
    
                | munchStm(T.MOVE(T.MEM(e1),e2)) =                   
                    emit(Assem.OPER{assem="sw `s1, 0(`s0)\n",
                        src=[munchExp(e1), munchExp(e2)],
                        dst=[], 
                        jump=NONE
                    })
                | munchStm(T.MOVE(T.TEMP(i), e2) ) = 
                    emit(Assem.MOVE{
                        assem = "move `d0, `s0\n",
                        src = munchExp(e2),
                        dst = i
                    })

            and munchExp(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) = 
                result(
                    fn r => emit(Assem.OPER{
                                assem = "lw `d0, " ^ int(i) ^ "(`s0)\n",
                                src = [munchExp(e1)],
                                dst = [r],
                                jump = NONE}))
            | munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) = 
                result(
                    fn r => emit(Assem.OPER{
                                assem = "lw `d0, " ^ int(i) ^ "(`s0)\n",
                                src = [munchExp(e1)],
                                dst = [r],
                                jump = NONE}))
            | munchExp(T.MEM(T.BINOP(T.MINUS, e1, T.CONST i))) = munchExp(T.MEM(T.BINOP(T.PLUS, e1, T.CONST (~i))))
            | munchExp(T.MEM(e1)) =
                 result(
                        fn r => emit(Assem.OPER{
                                assem = "lw `d0, 0(`s0)\n",
                                src = [munchExp(e1)],
                                dst = [r],
                                jump = NONE}))
            | munchExp(T.BINOP(T.PLUS, e1, T.CONST(i))) = 
                result(
                    fn r => emit(Assem.OPER({
                                assem = "addi `d0, `s0, " ^ int(i) ^ "\n",
                                src = [munchExp(e1)],
                                dst = [r],
                                jump = NONE})))
            | munchExp(T.BINOP(T.AND, e1, T.CONST(i))) = 
                result(
                    fn r => emit(Assem.OPER({
                                assem = "andi `d0, `s0, " ^ int(i) ^ "\n",
                                src = [munchExp(e1)],
                                dst = [r],
                                jump = NONE})))
            | munchExp(T.BINOP(T.OR, e1, T.CONST(i))) = 
                result(
                    fn r => emit(Assem.OPER({
                                assem = "ori `d0, `s0, " ^ int(i) ^ "\n",
                                src = [munchExp(e1)],
                                dst = [r],
                                jump = NONE})))
            | munchExp(T.BINOP(T.XOR, e1, T.CONST(i))) = 
                result(
                    fn r => emit(Assem.OPER({
                                assem = "xori `d0, `s0, " ^ int(i) ^ "\n",
                                src = [munchExp(e1)],
                                dst = [r],
                                jump = NONE})))
			| munchExp (T.BINOP(binop, e1, e2)) = 
                result (
                    fn r => emit (Assem.OPER{
                                assem=binopToMips(binop) ^ " `d0, `s0, `s1\n",
                                src=[munchExp(e1), munchExp(e2)], 
                                dst=[r], 
                                jump=NONE }))
            | munchExp(T.NAME(lab)) = 
                result(
                    fn r => emit(Assem.OPER{
                                assem = "la `d0, " ^ Symbol.name(lab) ^ "\n",
                                src = [],
                                dst = [r],
                                jump = NONE}))
            | munchExp(T.TEMP(t)) = t
            | munchExp(T.CONST(i)) = 
                result(
                fn r => emit(Assem.OPER({
                    assem = "li `d0, " ^ int(i) ^ "\n",
                    src = [],
                    dst = [r],
                    jump = NONE})))
            | munchExp(T.CALL(T.NAME(lab), args)) = 
                (emit(Assem.OPER({
                                    assem = "jal " ^ Symbol.name(lab) ^ "\n",
                                    src = munchArgs(0,args),
                                    dst = Frame.getCallRegisters(),
                                    jump = NONE}));
                Frame.RV)
            | munchExp(_) = (ErrorMsg.impossible "Can not select mips instructions"; Frame.RV)
            and munchArgs(index,[]) = []
            | munchArgs(index, a::l) = 
                let 
                    val  fp = T.BINOP(T.PLUS, T.TEMP(Frame.SP), T.CONST(index*4 + Frame.wordSize))
                in
                    if index < 4
                    then  (munchStm(T.MOVE(T.TEMP(List.nth(Frame.getArgRegs(), index)),a)); List.nth(Frame.getArgRegs(), index)::munchArgs(index+1,l) )
                    else (
                        
                        munchStm(T.MOVE(
                            T.MEM(fp),a
                        ));
                        munchArgs(index+1,l)
                    )
                end
        in 
        (
            munchStm stm;
            rev(!ilist)
        )
        end
end
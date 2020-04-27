structure MipsFrame : FRAME = struct

    datatype access = InFrame of int
        | InReg of Temp.temp

    type frame = {name: Temp.label, formals: access list, locals: int ref, offset: int ref}

    (* Always 0 *)
    val ZERO = Temp.newtemp()
    (*Assembler Temporary*)
    val AT = Temp.newtemp()
    (* Returned value of subroutines*)
    val RV = Temp.newtemp()
    val v1 = Temp.newtemp()
    (*First 4 arguments for subroutine calls*)
    val a0 = Temp.newtemp()
    val a1 = Temp.newtemp()
    val a2 = Temp.newtemp()
    val a3 = Temp.newtemp()
    (*Temporary registers*)
    val t0 = Temp.newtemp()
    val t1 = Temp.newtemp()
    val t2 = Temp.newtemp()
    val t3 = Temp.newtemp()
    val t4 = Temp.newtemp()
    val t5 = Temp.newtemp()
    val t6 = Temp.newtemp()
    val t7 = Temp.newtemp()
    val t8 = Temp.newtemp()
    val t9 = Temp.newtemp()
    (*Saved registers*)
    val s0 = Temp.newtemp()
    val s1 = Temp.newtemp()
    val s2 = Temp.newtemp()     
    val s3 = Temp.newtemp()
    val s4 = Temp.newtemp()
    val s5 = Temp.newtemp()
    val s6 = Temp.newtemp()
    val s7 = Temp.newtemp()
    (*Kernel Reserved Registers*)
    val k0 = Temp.newtemp()
    val k1 = Temp.newtemp()
    (*Globals pointer*)
    val GP = Temp.newtemp()
    (*Stack pointer*)
    val SP = Temp.newtemp()
    (*Frame pointer*)
    val FP = Temp.newtemp()
    (*Return address*)
    val RA = Temp.newtemp()
    
    type register = string
    val wordSize = 4  
    val k = 4
            
    val callersaves = [(t0,"$t0"), (t1,"$t1"), (t2,"$t2"), (t3, "$t3"), (t3,"$t4"), (t5,"$t5"), (t6, "$t6"), (t7,"$t7"), (t8,"$t8"), (t9, "$t9")]
    val calleesaves = [(s0,"$s0"), (s1,"$s1") , (s2, "$s2"), (s3, "$s3"), (s4, "$s4"), (s5,"$s5"), (s6,"$s6"), (s7,"$s7")]
    val specialregs = [(ZERO,"$zero"), (AT,"$at"), (RV, "$v0"), (v1,"$v1"), (k0,"$k0"), (k1,"$k1"), (GP, "$gp"), (SP, "$sp"), (FP, "$fp"), (RA, "$ra") ]
    val argregs = [(a0,"$a0"),(a1, "$a1"),(a2, "$a2"),(a3, "$a3")]

    (*For ease of incorporating canon into codebase*)
    fun stmListToSeq ([]) =  Tree.EXP(Tree.CONST(0))
    | stmListToSeq([a]) =  a
    | stmListToSeq(a::l) = Tree.SEQ(a,stmListToSeq(l)) 
    
    fun extractFirst(a,b) = a

    fun extractSecond(a,b) = b

    fun getCallerSaves() = map extractFirst callersaves

    fun getCalleeSaves() = map extractFirst calleesaves
 
    fun getSpecialRegs() = map extractFirst specialregs

    fun getArgRegs() = map extractFirst argregs

    fun getCallRegisters() = [RA,RV,v1] @  getCallerSaves() @ getArgRegs()

    fun colorableRegisters() = map extractSecond (callersaves @ calleesaves) @ ["$v0", "$v1"]

    val tempMap = 
        let
            fun help((curTemp,curString), curTable) = Temp.Map.insert(curTable,curTemp,curString)
        in 
            foldl help Temp.Map.empty (callersaves @ calleesaves @ specialregs @ argregs)
        end 

    fun makestring(temp) = (case Temp.Map.find(tempMap,temp) of 
        SOME(regName) => regName
        | _ => Temp.makestring(temp)
    )

    fun removeEscapeSeq(#"\n") = "\\n"
      | removeEscapeSeq(#"\t") = "\\t"
      | removeEscapeSeq(c) = Char.toString(c)

    fun string(lab, s) = Symbol.name(lab) ^ ":\n .word " ^ Int.toString(String.size(s)) ^ "\n .ascii \"" ^ String.translate(removeEscapeSeq)(s) ^ "\"\n"

    fun name({name,formals,locals,offset})  = name

    fun formals({name,formals,locals,offset}) = formals

    fun exp(InReg(reg))(address) = Tree.TEMP(reg) 
    | exp(InFrame(k))(address) =  Tree.MEM(Tree.BINOP(Tree.PLUS,address,Tree.CONST(k)))

    fun externalCall(s,args) = Tree.CALL(Tree.NAME(Temp.namedlabel(s)), args)

    datatype frag = PROC of {body: Tree.stm, frame:frame}
        | STRING of Temp.label * string

    fun procEntryExit1 (frame as {name,formals,locals,offset}, body ) =
        let
            fun moveFormals(index) =
                if index >= length(formals)
                then []
                else 
                    if index < 4
                    then [Tree.MOVE(exp(List.nth(formals,index))(Tree.TEMP(FP)), Tree.TEMP(List.nth(getArgRegs(), index)))] @ moveFormals(index + 1)    
                    else case List.nth(formals,index) of
                            InFrame(_) => moveFormals(index + 1)
                            | InReg(t) => [Tree.MOVE(exp(List.nth(formals,index))(Tree.TEMP(FP)), Tree.TEMP(t))] @ moveFormals(index + 1) 
        in
            stmListToSeq(moveFormals(0) @ [body])
        end

    fun procEntryExit2 ({name, formals, locals, offset}, body) = body @ [Assem.OPER{assem="", src= map extractFirst (specialregs), dst=[],jump=SOME[]}]

    fun int(i) =  
        if i< 0
        then "-" ^ int(~i)
        else Int.toString(i)

    fun procEntryExit3 ({name, formals, locals, offset}, body) =
        let

            val regsToSave = RA::getCalleeSaves()
            val label = Assem.LABEL{assem = Symbol.name(name) ^ ":\n", lab = name}
            val moveSL = Assem.OPER{assem = "move `s0, `d0\n", src = [a0], dst = [FP], jump=NONE}
            val storeFP = Assem.OPER{assem = "sw `s0, -4(`s1)\n", src = [FP, SP], dst = [], jump = NONE}
            val copySPtoFP = Assem.OPER{assem = "move `d0, `s0\n", src = [SP], dst = [FP], jump = NONE}
            val moveSP = Assem.OPER{assem="addi `d0, `s0, -" ^ int(abs(!offset + wordSize * (length(regsToSave) + 1))) ^ "\n", src = [FP], dst = [SP], jump = NONE}

            fun storeLoads([], moves, _, isStore) = moves
              | storeLoads(temp::temps, moves, offset, isStore) =
                if isStore
                then (Assem.OPER{assem="sw `s0, " ^ (int(offset)) ^ "(`s1)\n", src = [temp, FP], dst = [], jump = NONE})::storeLoads(temps, moves, offset - wordSize, isStore)
                else (Assem.OPER{assem="lw `d0, " ^ (int(offset)) ^ "(`s0)\n", src = [FP], dst = [temp], jump=NONE})::storeLoads(temps, moves, offset - wordSize, isStore)
              
            val stores = storeLoads(regsToSave, [],  ~8, true)
            val loads = storeLoads(regsToSave, [],  ~8,false)

            val moveSPtoFP = Assem.OPER {assem="move `d0, `s0\n", src = [FP], dst = [SP], jump = NONE}
            val getFp = Assem.OPER{assem = "lw `d0, -4(`s0)\n", src = [FP], dst = [FP], jump = NONE}
            val return = Assem.OPER{assem = "jr `d0\n", src = [], dst = [RA], jump = NONE}

            val prolog = [label] @ (if(name = Symbol.symbol "tig_main") then [moveSL] else []) @ [storeFP] @ [copySPtoFP] @ [moveSP] @ stores
            val epilog = loads @ [moveSPtoFP] @ [getFp]@ [return]

        in
            {prolog = "PROCEDURE " ^ Symbol.name name ^ "\n",
             body = prolog @ body @ epilog,
             epilog = "END " ^ Symbol.name name ^ "\n"}
        end



    fun allocLocal({name,formals,locals,offset})(escape) =
        (locals:= !locals+1;
        if escape 
        then  (offset:= !offset + wordSize; InFrame(!offset))
        else InReg(Temp.newtemp()))
        
    fun getOffset(InFrame(k)) = k
    | getOffset(_) = ErrorMsg.impossible "Getting offset of something stored in register" 0
        
    fun newFrame({name,formals}) = 
        let
            val stackCount = ref 0
            val regCount = ref 0

            fun allocLocal([],formals') = formals'
            | allocLocal(a::l, formals') = 
                (*if it escapes must allocate on the stack*)
                if a 
                then
                    (stackCount:= !stackCount+1; allocLocal(l, formals' @ [InFrame((!stackCount-1)*wordSize)]   ))
                else
                    (*if we haven't used k registers yet we can allocate to a register*)
                    if !regCount >= k
                    then (stackCount:= !stackCount+1; allocLocal(l,  formals'@ [InFrame((!stackCount-1)*wordSize)]  ))
                    else (regCount:= !regCount+1 ;allocLocal(l,  formals' @ [InReg(Temp.newtemp())]))
            
            val formals' = allocLocal(formals,[])
            val offset = !stackCount*wordSize
        in
            {name = name, formals = formals', locals = ref 0, offset = ref offset}
        end

end


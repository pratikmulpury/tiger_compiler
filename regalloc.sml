signature REG_ALLOC = 
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Map.map
    val alloc: Assem.instr list * Frame.frame -> Assem.instr list * allocation
end 

structure RegAlloc : REG_ALLOC = 
struct 
    structure Frame = MipsFrame
    type allocation = Frame.register Temp.Map.map

    fun removeMoves(instrs, allocation) =
        let
            fun keepInstr(instr) = case instr of
                                          Assem.MOVE{assem, dst, src} => valOf(Temp.Map.find(allocation, dst)) <> valOf(Temp.Map.find(allocation, src)) 
                                        | _ => true
        in
            List.filter keepInstr instrs
        end

    fun alloc(instrs, frame as {name,formals,locals,offset}) = 
        let
            fun rewriteProgram(instrs, spills) = 
                let
                    fun oneSpill(temp, instrs) = 
                        let
                            val access = Frame.allocLocal(frame)(true)
                            val curOff = Frame.getOffset(access)

                            val fetch = "lw `d0, " ^ Int.toString(curOff) ^ "($fp)\n"
                            val store = "sw `s0, " ^ Int.toString(curOff) ^ "($fp)\n"

                            fun genStore temp = Assem.OPER{ assem = fetch, dst = [temp], src = [], jump = NONE }

                            fun genFetch temp = Assem.OPER{assem =  store,  dst = [], src = [temp], jump = NONE }


                            fun containsFlow([]) = false
                            | containsFlow(a::l) = 
                                if a = temp
                                then true
                                else containsFlow(l)      

                            fun replace (x, []) = []
                            | replace (x, a::l) = 
                                if a = temp 
                                then x::replace(x, l)
                                else a::replace(x, l)
            
                            fun modifyInstrs([], rewrittenInstructions) = rewrittenInstructions
                                | modifyInstrs(a::l, rewrittenInstructions) = 

                                    let
                                        val t = Temp.newtemp()
                                        val t' = Temp.newtemp()
                                    in
                                        (
                                        case a of Assem.LABEL{assem, lab} => modifyInstrs(l, [a] @ rewrittenInstructions)
                                        | Assem.OPER{assem,dst,src,jump} =>
                                            if containsFlow(dst) andalso containsFlow(src)
                                            then  modifyInstrs(l, rewrittenInstructions @ genFetch t :: Assem.OPER{assem=assem, dst =replace(t',dst), src = replace(t,src), jump = jump} :: [genStore t'])
                                            else if containsFlow(dst) andalso not(containsFlow(src))
                                            then  modifyInstrs(l, rewrittenInstructions @ Assem.OPER{assem=assem, dst =replace(t,dst), src = src, jump = jump} :: [genStore t])
                                            else if not(containsFlow(dst)) andalso containsFlow(src)
                                            then modifyInstrs(l, rewrittenInstructions @ genFetch t :: [Assem.OPER{assem=assem, dst =dst, src = replace(t,src), jump = jump}])
                                            else modifyInstrs(l, rewrittenInstructions @ [a])
                                        | Assem.MOVE{assem,dst,src} =>
                                            if containsFlow([dst]) andalso containsFlow([src])
                                            then  modifyInstrs(l, rewrittenInstructions @ genFetch t :: Assem.MOVE{assem=assem, dst = List.nth(replace(t',[dst]),0), src =  List.nth(replace(t,[src]),0)  } :: [genStore t'])
                                            else if containsFlow([dst]) andalso not(containsFlow([src]))
                                            then  modifyInstrs(l, rewrittenInstructions @ Assem.MOVE{assem=assem, dst = List.nth(replace(t,[dst]),0), src = src} :: [genStore t])
                                            else if not(containsFlow([dst])) andalso containsFlow([src])
                                            then modifyInstrs(l, rewrittenInstructions @ genFetch t :: [Assem.MOVE{assem=assem, dst =dst, src = List.nth(replace(t,[src]),0)}])
                                            else modifyInstrs(l, rewrittenInstructions @ [a])
                                        )
                                    end
                        in
                            modifyInstrs(instrs,[])
                        end
                in
                    foldl oneSpill instrs spills
                end


            val (flowgraph,nodes) = MakeGraph.instrs2graph instrs
            val (interferenceGraph, liveOuts) = Liveness.interferenceGraph flowgraph

            (* implement one from book*)
            fun spillcost x = 1.0

            (* val _ = Liveness.show(TextIO.stdOut, igraph) *)
            val (allocation, spills) = Color.color({
                                        interference = interferenceGraph,
                                        spillCost = spillcost, 
                                        registers = Frame.colorableRegisters()})

            val instrs' = if List.length(spills) = 0
                          then removeMoves(instrs, allocation)
                          else instrs
        in
            if List.length(spills) = 0
            then (instrs', allocation)
            else alloc(rewriteProgram(instrs, spills),frame)
        end

end
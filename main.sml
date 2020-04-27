structure Main = struct

    fun emitproc out (MipsFrame.PROC{body,frame}) =
            let
                val stms = Canon.linearize body
                val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
                val instrs =   List.concat(map (MipsGen.codegen frame) stms')
                val instrs' = MipsFrame.procEntryExit2(frame, instrs)
                val (instrs'', allocation) = RegAlloc.alloc(instrs', frame)
                val instrs''' = #body(MipsFrame.procEntryExit3(frame, instrs''))
                fun formatHelp temp = valOf(Temp.Map.find(allocation,temp))
                val format = Assem.format(formatHelp)
            in
                app (fn i => TextIO.output(out,format i)) instrs'''
            end
      | emitproc out (MipsFrame.STRING(lab,s)) = (TextIO.output(out, MipsFrame.string(lab,s)))

  fun withOpenFile fname f =
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out)
	    handle e => (TextIO.closeOut out; raise e)
       end
       
   fun compile filename =
        let
            val absyn = Parse.parse filename
            val _ = FindEscape.findEscape absyn
            val frags = Semant.transProg absyn
            fun isStringFrag(MipsFrame.STRING(_)) = true
            | isStringFrag x = false
            fun notString(MipsFrame.STRING(_)) = false
            | notString x = true
            val strings = List.filter isStringFrag frags
            val procs = List.filter notString frags
            (*i should probably format this better at some point*)
            fun outFun out = 
                let 
                    val _ = TextIO.output(out, ".globl tig_main \n")
                    val _ = TextIO.output(out, ".data\n")
                    val _ =app (emitproc out) strings
                    val _ =TextIO.output(out, ".text\n");
                    val _ = app (emitproc out) procs
                    val runtime = TextIO.openIn "runtimele.s"
                    val _ = TextIO.output(out, (TextIO.inputAll runtime))
                    val _ = TextIO.closeIn runtime
                    val sysspim = TextIO.openIn "sysspim.s"
                    val _ = TextIO.output(out, (TextIO.inputAll sysspim))
                    val _ = TextIO.closeIn sysspim
                in
                    ()
                end
        in
            withOpenFile (filename ^ ".s") outFun
       end
end


signature FRAME =
sig 
    type frame
    type access 
    type register
        
    val RV: Temp.temp
    val FP: Temp.temp
    val SP: Temp.temp
    val wordSize: int
    val exp: access -> Tree.exp -> Tree.exp 
    val k: int
    val newFrame : {name: Temp.label,formals: bool list} -> frame
    val name : frame -> Temp.label
    val getOffset: access -> int
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
    val externalCall : string * Tree.exp list -> Tree.exp
    val string: Temp.label * string -> string
    val getArgRegs: unit -> Temp.temp list
    val getCallRegisters: unit -> Temp.temp list
    val tempMap : register Temp.Map.map
    val makestring: Temp.temp -> string
    val colorableRegisters: unit -> register list
    val procEntryExit1: frame * Tree.stm -> Tree.stm
    val procEntryExit2: frame * Assem.instr list -> Assem.instr list
    val procEntryExit3: frame * Assem.instr list -> {prolog: string, body: Assem.instr list, epilog: string}

    datatype frag = PROC of {body: Tree.stm, frame:frame}
        | STRING of Temp.label * string
end
signature TRANSLATE =
sig
    type access
    type level

    datatype exp = Ex of Tree.exp
                 | Nx of Tree.stm
                 | Cx of Temp.label * Temp.label -> Tree.stm

    val outermost: level    
    structure Frame: FRAME 
    type frag
    val fragList: Frame.frag list ref    
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level->bool -> access
    val reinitializeFrags: unit -> unit
    val unEx: exp -> Tree.exp
    val unNx: exp -> Tree.stm
    val unCx : exp -> (Temp.label * Temp.label -> Tree.stm) 
    val stmListToSeq: Tree.stm list -> Tree.stm
    val abOpToBinop: Absyn.oper -> Tree.binop
    val abOpToRelop: Absyn.oper -> Tree.relop

    val simpleVar : access * level -> exp
    val fieldVar : exp * int -> exp
    val subscriptVar:  exp * exp -> exp
    val arithmetic: Absyn.oper * exp * exp -> exp
    val conditional: Absyn.oper * exp * exp * Types.ty -> exp
    val ifExp: exp *exp -> exp
    val ifThenElseExp: exp*exp *exp -> exp
    val seqExp: exp list -> exp
    val letExp: exp list * exp -> exp
    val nilExp: unit -> exp
    val intExp: int -> exp
    val stringExp:string -> exp
    val recordExp: exp list -> exp
    val arrayExp: exp * exp -> exp
    val whileExp:  exp * exp * Temp.label -> exp
    val forExp: exp * exp * exp * exp * Temp.label -> exp
    val breakExp: Temp.label -> exp
    val callExp: Temp.label * level * level * (exp list) -> exp
    val getResult : unit -> frag list
    val assignExp: exp * exp -> exp
    val procEntryExit: {level: level, body:exp} -> unit
end

structure Translate : TRANSLATE =
struct

  structure Frame : FRAME = MipsFrame

  datatype level = TOP
                 | LEVEL of {frame: Frame.frame, parent: level, unique: unit ref}

  datatype exp = Ex of Tree.exp
              |  Nx of Tree.stm
              |  Cx of Temp.label * Temp.label -> Tree.stm

  type frag = Frame.frag

  type access = level * Frame.access
  val fragList = (ref []): Frame.frag list ref


  val outermost = TOP

  fun newLevel({parent,name,formals}) = LEVEL{parent=parent, frame = Frame.newFrame({name=name, formals = true::formals}), unique = ref() }

  fun stmListToSeq ([]) =  Tree.EXP(Tree.CONST(0))
  | stmListToSeq([a]) =  a
  | stmListToSeq(a::l) = Tree.SEQ(a,stmListToSeq(l)) 

  fun formals TOP = []
      | formals(LEVEL{parent,frame,unique}) = 
      let
          val level = LEVEL{parent = parent, frame = frame, unique = unique}
          val formals' = Frame.formals(frame)
          fun formAccess(acc) = (level,acc)
          val formals'' = map  formAccess formals'
      in
          formals''
      end

  fun allocLocal(TOP)(escape) = (outermost,  Frame.allocLocal(Frame.newFrame({name=Temp.newlabel(),formals = []}))(escape) ) 
      | allocLocal(LEVEL{parent,frame,unique})(escape)  = (LEVEL{parent=parent,frame=frame,unique=unique}, Frame.allocLocal(frame)(escape)) 

  and unEx(Ex e) = e
    | unEx (Nx(Tree.EXP(e))) = e
    | unEx (Nx(Tree.SEQ(stm, Tree.EXP(exp)))) = Tree.ESEQ(stm, exp)
    | unEx (Nx(Tree.SEQ(stm, Tree.SEQ(stms)))) = Tree.ESEQ(stm, unEx(Nx(Tree.SEQ(stms))))
    | unEx (Nx s) = Tree.ESEQ(s, Tree.CONST(0))
    | unEx(Cx genstm) =
        let
          val r = Temp.newtemp()
          val t = Temp.newlabel() 
          val f = Temp.newlabel()
        in
            Tree.ESEQ(stmListToSeq[Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
                          genstm(t,f),
                          Tree.LABEL f,
                          Tree.MOVE(Tree.TEMP r, Tree.CONST 0),
                          Tree.LABEL t],
                      Tree.TEMP r)
        end
  fun unNx (Nx s) = s
      | unNx(Cx genstm) = unNx(Ex(unEx(Cx(genstm))))
      | unNx(Ex e) = Tree.EXP(e)
  
  fun unCx(Cx genstm) = genstm
      | unCx(Ex(Tree.CONST(0))) = 
          let
              fun genstm(t,f) =  Tree.JUMP(Tree.NAME f, [f])
          in
              genstm
          end
      | unCx(Ex(Tree.CONST(1))) =
          let
            fun genstm(t,f) = Tree.JUMP(Tree.NAME t, [t])
          in
            genstm
          end
      | unCx(Ex e) = 
          let 
              fun genstm(t,f) = Tree.CJUMP(Tree.EQ,Tree.CONST(1),e,t,f)
          in
              genstm
          end
      | unCx(Nx s ) = 
          let
            fun genstm(t,f) = Tree.JUMP(Tree.NAME t,[t])
          in
              (ErrorMsg.error 0 "Not a well typed tiger program"; genstm)
          end

 fun followLinks(TOP,_) = (ErrorMsg.error 0 "Variable was declared in outer level";  Tree.TEMP(Frame.FP))
  | followLinks(_,TOP) = (ErrorMsg.error 0 "Level where variable was declared not found in static links"; Tree.TEMP(Frame.FP))
  | followLinks(targetLevel as LEVEL{parent = targetParent , frame = targetFrame, unique = targetRef },currentLevel as LEVEL{parent = curParent, frame = curFrame, unique = curRef}) = 
      if targetRef = curRef
      then Tree.TEMP(Frame.FP)
      else Tree.MEM(followLinks(targetLevel,curParent))

fun simpleVar((decLevel,access),useLevel) = Ex(Frame.exp(access)(followLinks(decLevel,useLevel)))

fun fieldVar(record,index) = Ex(Tree.MEM(Tree.BINOP(Tree.PLUS, unEx(record), Tree.CONST(index*Frame.wordSize))))

fun subscriptVar(variable,index) = 
    let
        val indexRegister = Temp.newtemp()
        val sizeRegister = Temp.newtemp()
        val variableRegister =  Temp.newtemp()
        val upperBoundCheck = Temp.newlabel()
        val outOfBounds = Temp.newlabel()
        val inBounds = Temp.newlabel()
        (* TODO - Need to check upper bound - add size of array to array*)
    in
        Ex(Tree.ESEQ(stmListToSeq[    
        Tree.MOVE(Tree.TEMP(variableRegister), unEx(variable)),
        Tree.MOVE(Tree.TEMP(indexRegister), unEx(index)),
        Tree.CJUMP(Tree.GE, Tree.TEMP(indexRegister), Tree.CONST(0),upperBoundCheck,outOfBounds),
        Tree.LABEL(upperBoundCheck),
        Tree.MOVE(Tree.TEMP(sizeRegister), Tree.MEM( Tree.BINOP(Tree.MINUS,unEx(variable), Tree.CONST(Frame.wordSize)))),
        Tree.CJUMP(Tree.GT, Tree.TEMP(sizeRegister), Tree.TEMP(indexRegister), inBounds, outOfBounds),
        Tree.LABEL(outOfBounds), 
        Tree.EXP(Frame.externalCall("tig_exit", [Tree.CONST(0)] )),
        Tree.LABEL(inBounds)
        ], Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP(variableRegister), Tree.BINOP(Tree.MUL, Tree.TEMP(indexRegister), Tree.CONST(Frame.wordSize)))))) 
    end

  fun abOpToBinop(operation) = 
  ( case operation of Absyn.PlusOp => Tree.PLUS
  | Absyn.MinusOp => Tree.MINUS
  | Absyn.TimesOp => Tree.MUL
  | Absyn.DivideOp => Tree.DIV
  | _ => Tree.PLUS)

  fun abOpToRelop(operation) = (case operation of Absyn.EqOp => Tree.EQ
  | Absyn.NeqOp => Tree.NE
  | Absyn.LtOp => Tree.LT
  | Absyn.LeOp => Tree.LE
  | Absyn.GtOp => Tree.GT
  | Absyn.GeOp => Tree.GE
  | _ => Tree.NE)

  fun arithmetic(oper, exp1 , exp2) =  Ex(Tree.BINOP(abOpToBinop(oper), unEx(exp1), unEx(exp2)))

  fun conditional(oper,exp1,exp2,ty) = 
      let 
          fun con(t,f) =  Tree.CJUMP(abOpToRelop(oper), unEx(exp1), unEx(exp2), t,f)
      in
      (case ty of 
          Types.STRING =>
              (case abOpToRelop(oper) of 
                Tree.EQ => Ex(Frame.externalCall("tig_stringEqual", [unEx(exp1), unEx(exp2)]) )
              | Tree.NE => Ex(Frame.externalCall("stringNE", [unEx(exp1), unEx(exp2)]) )
              | Tree.LT => Ex(Frame.externalCall("stringLT", [unEx(exp1), unEx(exp2)]) )
              | Tree.GT => Ex(Frame.externalCall("stringGT", [unEx(exp1), unEx(exp2)]) )
              | Tree.LE => Ex(Frame.externalCall("stringLE", [unEx(exp1), unEx(exp2)]) )
              | Tree.GE => Ex(Frame.externalCall("stringGE", [unEx(exp1), unEx(exp2)]) )
              | _ =>  Cx (con)
              )
          | _ => Cx (con)
      )
      end

  fun ifExp(ifExp, thenExp) = 
      let 
          val ifExpCx = unCx(ifExp)
          val thenExpEx = unEx(thenExp)
          val t = Temp.newlabel()
          val f = Temp.newlabel()
          val r = Temp.newtemp()
          val join = Temp.newlabel()
      in
          Ex(Tree.ESEQ(stmListToSeq[
              ifExpCx(t,f),
              Tree.LABEL(t),
              Tree.MOVE(Tree.TEMP(r), thenExpEx),
              Tree.JUMP(Tree.NAME join,[join]),
              Tree.LABEL(f),
              Tree.LABEL(join)
          ],
          Tree.TEMP(r)))
      end

  fun ifThenElseExp(ifExp, thenExp, elseExp) = 
      let 
          val ifExpCx = unCx(ifExp)
          val thenExpEx = unEx(thenExp)
          val elseExpEx = unEx(elseExp)
          val t = Temp.newlabel()
          val f = Temp.newlabel()
          val r = Temp.newtemp()
          val join = Temp.newlabel()
      in
          Ex(Tree.ESEQ(stmListToSeq[
              ifExpCx(t,f),
              Tree.LABEL(t),
              Tree.MOVE(Tree.TEMP(r), thenExpEx),
              Tree.JUMP(Tree.NAME join,[join]),
              Tree.LABEL(f),
              Tree.MOVE(Tree.TEMP(r), elseExpEx),
              Tree.LABEL(join)
          ],
          Tree.TEMP(r)))
      end
  fun nilExp() = Ex(Tree.CONST(0))

  fun intExp(int) = Ex(Tree.CONST(int))

  fun seqExp([]) = Ex(Tree.CONST(0))
      | seqExp([a]) = a
      | seqExp(a::l) = Ex(Tree.ESEQ(unNx(a), unEx(  seqExp(l)  ) )  )

  fun letExp(decs,body) = seqExp(decs @ [body])

  fun stringExp(string) =
      let
          val fragListActual = !fragList
          fun matchingStringLabel( string, []) = NONE
              | matchingStringLabel(string, a::l) = (case a of Frame.STRING(label',cur) => if string = cur then SOME(label')  else matchingStringLabel(string,l)
                                                    | _ => NONE)

          val label = (case matchingStringLabel(string,!fragList) of SOME(label') => (label')
                      | _ => (let val label' = Temp.newlabel() in (fragList:= Frame.STRING(label',string) :: (!fragList); label') end) )
      in
          Ex(Tree.NAME(label))
      end

  (* Double CHECK!!*)
  fun recordExp (fields) = 
    let 
      val r = Temp.newtemp()
      val numFields = List.length(fields)

      fun initialize(initializations,0) = initializations
                | initialize(initializations,initleft) = initialize([Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS,Tree.TEMP(r), Tree.CONST( (length(fields)-initleft) *Frame.wordSize))), unEx(List.nth(fields,length(fields)-initleft)))]  @  initializations, initleft-1)                                               

      val initializations = initialize([], length(fields))

    in
      Ex(Tree.ESEQ(
                stmListToSeq( [Tree.MOVE(Tree.TEMP(r), Frame.externalCall("tig_allocRecord", [Tree.CONST(length(fields)*Frame.wordSize)]))] @ initializations),
                Tree.TEMP(r))
            )    	
    end

  fun arrayExp(size,init) = 
      let
          val sizeRegister = Temp.newtemp()
          val startRegister = Temp.newtemp()
      in
        Ex(Tree.ESEQ(stmListToSeq([
            Tree.MOVE(Tree.TEMP(sizeRegister), Frame.externalCall("tig_initArray", [Tree.BINOP(Tree.PLUS, unEx(size), Tree.CONST(1)), unEx(init)])),
            Tree.MOVE(Tree.MEM(Tree.TEMP(sizeRegister)), unEx(size)),
            Tree.MOVE(Tree.TEMP(startRegister), Tree.BINOP(Tree.PLUS, Tree.TEMP(sizeRegister), Tree.CONST(Frame.wordSize))   )]), 
            Tree.TEMP(startRegister)))
      end

  (* Double CHECK!!*)


  fun breakExp(break) = Nx(Tree.JUMP(Tree.NAME(break), [break]))

  fun whileExp(test,body,breakLabel) = 
      let 
          val testLabel = Temp.newlabel()
          val bodyLabel = Temp.newlabel()
      in
          Nx(stmListToSeq([Tree.LABEL(testLabel),
          unCx(test)(bodyLabel,breakLabel),
          Tree.LABEL(bodyLabel),
          unNx(body),
          Tree.JUMP(Tree.NAME(testLabel), [testLabel]),
          Tree.LABEL(breakLabel)]))
      end
      
    fun forExp( indexVar ,lo, hi, body, breakLabel ) = 

        let
            val bodyLabel = Temp.newlabel()
            val incrementLabel = Temp.newlabel()
            val highRegister = Temp.newtemp()
        in
        Nx(stmListToSeq([
            Tree.MOVE(unEx(indexVar),unEx(lo)),
            Tree.MOVE(Tree.TEMP(highRegister), unEx(hi)),
            Tree.CJUMP(Tree.LE, unEx(indexVar), Tree.TEMP(highRegister),  bodyLabel, breakLabel),
            Tree.LABEL(bodyLabel),
            unNx(body),
            Tree.CJUMP(Tree.LT, unEx(indexVar), Tree.TEMP(highRegister),incrementLabel,breakLabel),
            Tree.LABEL(incrementLabel),
            Tree.MOVE(unEx(indexVar), (Tree.BINOP(Tree.PLUS, unEx(indexVar), Tree.CONST(1)))),
            Tree.JUMP(Tree.NAME bodyLabel,[bodyLabel]),
            Tree.LABEL(breakLabel)]))    
        end


    fun callExp(nameLabel, callLevel, decLevel, arguments) = 
        (case decLevel of TOP => Ex(Frame.externalCall( Symbol.name(nameLabel),  map unEx arguments))
                              | LEVEL{parent,frame,unique} => Ex(Tree.CALL(Tree.NAME(nameLabel), followLinks(parent,callLevel)::(map unEx arguments)))
        )
 


  fun procEntryExit({body = body,level = TOP}) =  
    let
      val frame = Frame.newFrame({ name = Temp.newlabel(), formals = []})
    in
      (ErrorMsg.error 0 "Calling proc entry exit for  afunction declared in outermost level"; fragList:= Frame.PROC( {frame =  frame, body = Tree.MOVE(Tree.TEMP(Frame.RV), unEx(Nx(Frame.procEntryExit1(frame,unNx(body)))))} ):: !fragList )
    end
  
  | procEntryExit( { body = body, level = LEVEL{frame,parent,unique} }) = fragList:= Frame.PROC({frame = frame, body  = Tree.MOVE(Tree.TEMP(Frame.RV), unEx(Nx(Frame.procEntryExit1(frame, unNx(body)))))  }) :: !fragList


  fun assignExp(variable,value) = Nx(Tree.MOVE(unEx(variable), unEx(value)))

  fun getResult() = !fragList

  fun reinitializeFrags() = fragList := []

end

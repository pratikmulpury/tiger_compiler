signature SEMANT =
sig
  type venv
  type tenv
  type expty
  val transExp : venv * tenv * Absyn.exp * Translate.level * Temp.label-> expty
  val transDec : venv * tenv * Absyn.dec * Translate.level * Temp.label * Translate.exp list -> {venv: venv, tenv: tenv, exps: Translate.exp list} 
  val transTy  :                          tenv * Absyn.ty  -> Types.ty
  val transProg:                                 Absyn.exp -> Translate.frag list
end

structure Semant :> SEMANT =
struct
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  (* Variables and functions to track depth for valid curLabel-statements *)
  val curLevel = ref 0

  (*If both types are subtypes of each other return either. If neither are subtypes returns bottom. Otherwise returns the supertype.*)
  fun getReturnType(t1,t2) = 
    if Types.isSubtype(t1,t2) 
    then
      t2
    else
      if Types.isSubtype(t2,t1)
      then t1
      else  Types.BOTTOM

  (* Return true if type int. Otherwise print an error message.*)
  fun checkInt({exp,ty}, pos) = 
    case ty of Types.INT => true
      | Types.BOTTOM => true
      | ty =>  (ErrorMsg.error pos ("Integer expected. " ^ Types.toString(ty) ^ " found."); false)

  (* Return true if element contained in list.*)
  (* TODO -> change list to set for performance*)
  fun contains(a::l, el) = Symbol.name(a) = Symbol.name(el) orelse contains(l,el)
      | contains([],_) = false

  (*Return true if type unit. Otherwise print an error message.*)
  fun checkUnit({exp,ty}, pos) = 
    case ty of Types.UNIT => true
    | Types.BOTTOM => true
    | ty => (ErrorMsg.error pos ("Unit Expected. " ^ Types.toString(ty) ^ " found."); false)

  (*Check if expty records be compared with in an inequality operator (GT,LT,etc)*)
	fun checkIneqComparable ({exp=left,ty=lefty}, {exp=right,ty=righty}, pos) = 
    case (lefty,righty) of (Types.INT, Types.INT) => ()
    | (Types.STRING, Types.STRING) => ()
    | (Types.BOTTOM,_) => ()
    | (_,Types.BOTTOM) => ()
    |(_,_) => (ErrorMsg.error pos ("Can not be compared with an inequality operator."))

  (*Check if expty records can be compared with in an equality operator (EQ,NEQ)*)
	fun checkEqualComparable ({exp=left,ty=lefty}, {exp=right,ty=righty}, pos) =
    case (lefty,righty) of (Types.INT, Types.INT) => ()
    | (Types.STRING, Types.STRING) => () 
    | (Types.RECORD(_,ref1), Types.RECORD(_,ref2)) => (
      if ref1=ref2 
      then () 
      else ErrorMsg.error pos ("Equality operators can only compare records of the same instance"))
    | (Types.ARRAY(_,ref1),Types.ARRAY(_,ref2)) => 
      (if ref1=ref2 
      then () 
      else ErrorMsg.error pos ("Equality operators can only compare arrays of the same instance"))
    | (Types.NIL,Types.RECORD(_)) => ()
    | (Types.RECORD(_),Types.NIL) => ()
    | (Types.NIL, Types.NIL) => ()
    | (Types.BOTTOM,_) => ()
    | (_, Types.BOTTOM) => ()
    | (_,_) => ErrorMsg.error pos ("Equality operator requires matching type")
   
  (*Skips past the names to the actuasl type*)
    fun actual_ty (Types.NAME(symbol ,ty),pos) =
      (case !ty of SOME nextTy => actual_ty(nextTy,pos)
        | NONE => (ErrorMsg.error pos ("Type not found in environment " ^ Symbol.name(symbol) ^ "."); Types.BOTTOM))
    | actual_ty (realtype,pos) = realtype


  fun transExp(venv, tenv, expression, level, curLabel) =
      let fun trexp (Absyn.OpExp{left, oper=Absyn.PlusOp, right,pos}) = 
            let 
              val leftExpTy = trexp left
              val rightExpTy = trexp right
            in
          
            (checkInt(leftExpTy, pos);  
              checkInt(rightExpTy, pos); 
              {exp = Translate.arithmetic(Absyn.PlusOp, #exp(leftExpTy), #exp(rightExpTy)), ty = Types.INT})
            end
        | trexp (Absyn.OpExp{left, oper=Absyn.MinusOp, right,pos}) = 
        
          let
            val leftExpTy = trexp left
            val rightExpTy = trexp right
          in
            checkInt(leftExpTy, pos);  
            checkInt(rightExpTy, pos); 
            {exp = Translate.arithmetic(Absyn.MinusOp, #exp(leftExpTy), #exp rightExpTy), ty = Types.INT} 
          end
        | trexp (Absyn.OpExp{left, oper=Absyn.TimesOp, right,pos}) =  
          let
            val leftExpTy = trexp left
            val rightExpTy = trexp right
          in
            (checkInt(leftExpTy, pos);  
            checkInt(rightExpTy, pos); 
            {exp = Translate.arithmetic(Absyn.TimesOp, #exp leftExpTy, # exp rightExpTy), ty = Types.INT}) 
          end
        | trexp (Absyn.OpExp{left, oper=Absyn.DivideOp, right,pos}) =  
          let
            val leftExpTy = trexp left
            val rightExpTy = trexp right
          in
            (checkInt(leftExpTy, pos);  
            checkInt(rightExpTy, pos); 
            {exp = Translate.arithmetic(Absyn.DivideOp, #exp leftExpTy, #exp rightExpTy), ty = Types.INT}) 
          end
        | trexp (Absyn.OpExp{left, oper=Absyn.EqOp, right,pos}) = 
          let 
            val leftExpTy = trexp left
            val rightExpTy = trexp right
          in
            (checkEqualComparable(leftExpTy, rightExpTy, pos);  
            {exp = Translate.conditional(Absyn.EqOp,#exp leftExpTy, #exp rightExpTy, #ty leftExpTy ), ty = Types.INT})
          end
        | trexp (Absyn.OpExp{left, oper=Absyn.NeqOp, right,pos}) =  
          let 
            val leftExpTy = trexp left
            val rightExpTy = trexp right
          in
            (checkEqualComparable(leftExpTy, rightExpTy, pos);  
            {exp = Translate.conditional(Absyn.NeqOp,#exp leftExpTy, #exp rightExpTy, #ty rightExpTy ), ty = Types.INT})
          end
        | trexp (Absyn.OpExp{left, oper=Absyn.LtOp, right,pos}) =  
          let 
            val leftExpTy = trexp left
            val rightExpTy = trexp right
          in
            (checkEqualComparable(leftExpTy, rightExpTy, pos);  
            {exp = Translate.conditional(Absyn.LtOp,#exp leftExpTy, #exp rightExpTy, #ty rightExpTy ), ty = Types.INT})
          end
        | trexp (Absyn.OpExp{left, oper=Absyn.LeOp, right,pos}) =  
          let 
            val leftExpTy = trexp left
            val rightExpTy = trexp right
          in
            (checkEqualComparable(leftExpTy, rightExpTy, pos);  
            {exp = Translate.conditional(Absyn.LeOp,#exp leftExpTy, #exp rightExpTy, #ty rightExpTy ), ty = Types.INT})
          end
        | trexp (Absyn.OpExp{left, oper=Absyn.GtOp, right,pos}) = 
          let 
            val leftExpTy = trexp left
            val rightExpTy = trexp right
          in
            (checkEqualComparable(leftExpTy, rightExpTy, pos);  
            {exp = Translate.conditional(Absyn.GtOp,#exp leftExpTy, #exp rightExpTy, #ty rightExpTy ), ty = Types.INT})
          end
        | trexp (Absyn.OpExp{left, oper=Absyn.GeOp, right,pos}) =  
          let 
            val leftExpTy = trexp left
            val rightExpTy = trexp right
          in
            (checkEqualComparable(leftExpTy, rightExpTy, pos);  
            {exp = Translate.conditional(Absyn.GeOp,#exp leftExpTy, #exp rightExpTy, #ty rightExpTy), ty = Types.INT})
          end
        | trexp (Absyn.VarExp(var)) = trvar var
        | trexp (Absyn.NilExp) = {exp = Translate.nilExp(), ty = Types.NIL}
        | trexp (Absyn.IntExp(int)) = {exp = Translate.intExp(int), ty = Types.INT}
        | trexp (Absyn.StringExp(string, pos)) = {exp = Translate.stringExp(string), ty = Types.STRING}
        | trexp (Absyn.CallExp{func, args, pos}) =
          let 
              fun compare_parameters(a1::l1, a2::l2) = (if Types.compare(actual_ty(a1,pos),actual_ty(a2,pos)) = Types.EQ orelse Types.compare(actual_ty(a1,pos),actual_ty(a2,pos)) = Types.SUBTYPE then () else (ErrorMsg.error pos ("Given Argument: " ^ Types.toString(a1) ^ " Expected: " ^ Types.toString(a2)));Types.compare(actual_ty(a1,pos) ,actual_ty(a2,pos)) = Types.EQ andalso compare_parameters(l1,l2))
              | compare_parameters ([],[]) = true
              | compare_parameters (l1,[]) = (false)
              | compare_parameters ([],l2) = (false)
              val argExpTy = map trexp args
          in
              case Symbol.look(venv,func) of NONE => (ErrorMsg.error pos ("Undefined function");  {exp = Translate.nilExp(), ty = Types.BOTTOM})
              | SOME (Env.VarEntry{ty,indexVar,access}) => (ErrorMsg.error pos ("Attempting to call a variable as a function"); {exp = Translate.nilExp(), ty = Types.BOTTOM})
              | SOME (Env.FunEntry{level=decLevel, label = funLabel, formals = formals ,result = result}) => (if compare_parameters( map #ty(argExpTy), formals ) then ({exp = Translate.callExp(funLabel, level,decLevel,map #exp(argExpTy)), ty=actual_ty(result,pos)}) else (ErrorMsg.error pos ("Function formals and given arguments do not match for function " ^ Symbol.name(func) ); {exp = Translate.nilExp(), ty = Types.BOTTOM}))
          end
        | trexp(Absyn.RecordExp{fields,typ,pos}) = 
            let 
                fun translateField(sym,exp,pos) = (sym, trexp(exp),pos)

                val translatedFields' = map translateField fields

                fun getTy(sym,exp:expty, pos) = (sym, #ty exp, pos)

                val translatedFields = map getTy translatedFields'

                fun translateFieldExp(sym,exp:expty, pos) =  #exp(exp)
                
                val translatedFieldExp =  map translateFieldExp translatedFields'

                fun checkFields(l1,[]) = false
                | checkFields([],l2) = false
                | checkFields([(sym,ty)],[(sym2,exp,pos2)]) = checkField(sym,sym2,ty, exp, pos2)
                | checkFields((sym,ty)::l1, (sym2,exp,pos2)::l2) = if checkField(sym,sym2,ty, exp, pos2) then checkFields(l1,l2) else false
                and checkField(sym,sym2,ty1,ty2,pos2) = 
                if Symbol.name(sym) = Symbol.name(sym2)
                then
                    if getReturnType(actual_ty(ty1,pos),actual_ty(ty2,pos)) <> Types.BOTTOM
                    then
                    true
                    else
                    (ErrorMsg.error pos2 ("Field type mismatch. Expected field type: " ^ Types.toString(actual_ty(ty1,pos)) ^ " Provided Field Type: " ^ Types.toString(actual_ty(ty2,pos)) ); false)
                else (ErrorMsg.error pos2 ("Field Name mismatch"); false)
            in
                case Symbol.look(tenv,typ) of NONE => (ErrorMsg.error pos ("Invalid Record Type"); {exp = Translate.nilExp(), ty = Types.BOTTOM})
                                        | SOME (ty) => (case actual_ty(ty,pos) of Types.RECORD(typeList,unique) => if checkFields(typeList,translatedFields) then {exp = Translate.recordExp(translatedFieldExp),ty = actual_ty(ty,pos)} else {exp = Translate.nilExp(), ty = Types.BOTTOM}
                                                                                | Types.BOTTOM => {exp = Translate.nilExp(), ty = Types.BOTTOM} 
                                                                                | _ => (ErrorMsg.error pos ("Invalid Record Type"); {exp = Translate.nilExp(), ty = Types.BOTTOM}))
            end
        | trexp(Absyn.SeqExp(expseq)) = 
                let fun trexpseq ([],l) = ([], Types.UNIT)
                | trexpseq ([(exp,pos)],l1) = (l1 @ [#exp(trexp exp)] , #ty(trexp exp))
                | trexpseq (((exp,pos)::l),l1) = trexpseq(l,  l1 @ [#exp(trexp exp)] )
                
                val (expList,expseqTy) = trexpseq(expseq,[])
            in
                {exp = Translate.seqExp(expList), ty= expseqTy}
            end      
        | trexp(Absyn.AssignExp{var,exp,pos}) = 
            let
                val translatedVar = trvar(var)
                val translatedExp = trexp(exp)    
                val indexVar =  case var of Absyn.SimpleVar(sym,pos) => (case Symbol.look(venv,sym) of SOME(Env.VarEntry{ty,indexVar,access}) => (if indexVar then ErrorMsg.error pos ("Can not assign to index variables") else (); indexVar)
                                                            | _ => (ErrorMsg.error pos("Variable is not registered in type"); true)
                                                            )
                                | _ => false
            in
                (
                if Types.compare(actual_ty( #ty (translatedVar), pos) ,actual_ty( #ty(translatedExp),pos)) = Types.EQ orelse Types.isSubtype( actual_ty(#ty(translatedExp), pos), actual_ty(#ty(translatedVar), pos) )
                then {exp = Translate.assignExp(#exp translatedVar, #exp translatedExp), ty = Types.UNIT}
                else (ErrorMsg.error pos ("Can only assign when types are equal"); {exp = Translate.nilExp(), ty = Types.UNIT}))
            end
    | trexp (Absyn.IfExp{test, then', else', pos}) = 
        let 
            val ifTranslated = trexp test
            val thenTranslated = trexp then' 
            val elseTranslated = (case else' of SOME(exp) => trexp exp
                                                | NONE => {exp = Translate.nilExp(), ty = Types.UNIT})
        in
            if checkInt(ifTranslated,pos)
            then case else' of 
            SOME (exp) => if getReturnType(actual_ty(#ty (elseTranslated),pos), actual_ty(#ty(thenTranslated),pos)) <> Types.BOTTOM 
            then {exp= Translate.ifThenElseExp(#exp ifTranslated, #exp thenTranslated, #exp elseTranslated), ty=getReturnType(actual_ty(#ty (elseTranslated),pos), actual_ty(#ty(thenTranslated),pos))} 
            else (ErrorMsg.error pos ("Then and Else of If then else statement expected to have same type. " ^ "Then Type: " ^ Types.toString(actual_ty(#ty (thenTranslated),pos)) ^" Else Type: " ^ Types.toString(actual_ty(#ty (elseTranslated),pos))); {exp = Translate.nilExp(), ty = Types.BOTTOM})
            | NONE => if checkUnit(thenTranslated, pos) then {exp = Translate.ifExp(#exp(ifTranslated), #exp(thenTranslated)), ty = Types.UNIT} else (ErrorMsg.error pos ("Body of If then statement expected to have unit value"); {exp = Translate.nilExp(), ty = Types.BOTTOM})
            else {exp = Translate.nilExp(), ty = Types.BOTTOM}   
        end
    | trexp(Absyn.WhileExp{test,body,pos}) = 
        let 
        val testTy = trexp(test)
        val inc = curLevel:= !curLevel+1
        val bodyTy = trexp(body)
        val dec = curLevel:= !curLevel-1
        val breakLabel = Temp.newlabel()
        val bodyExp = #exp(transExp( venv,tenv,body,level,breakLabel))

        in 
        (checkInt(testTy, pos); checkUnit(bodyTy,pos); {exp = Translate.whileExp(#exp testTy, bodyExp, breakLabel), ty = Types.UNIT})
        end
      | trexp(Absyn.ForExp{var,escape,lo,hi,body,pos}) = 
          let 
          val loTy = trexp lo;
          val hiTy = trexp hi
          val indexVarAccess = Translate.allocLocal(level)(!escape)
          val venv' = Symbol.enter(venv,var,Env.VarEntry{ty=Types.INT,indexVar = true,access = indexVarAccess })
          val breakLabel = Temp.newlabel()
          val bodyExp = transExp(venv',tenv,body,level,breakLabel)
          in 
          (
              if checkInt(loTy,pos) andalso checkInt(hiTy,pos)
              then
                  (curLevel:= !curLevel+1; checkUnit(bodyExp,pos); curLevel:= !curLevel-1; 
                  {exp = Translate.forExp( Translate.simpleVar(indexVarAccess,level), #exp loTy, #exp hiTy, #exp bodyExp, breakLabel), ty = Types.UNIT})
              else (ErrorMsg.error pos ("For loop bounds should be ints"); {exp = Translate.nilExp(), ty = Types.BOTTOM})
          )
          end
        | trexp (Absyn.BreakExp(pos)) =
                (if !curLevel = 0
                  then ErrorMsg.error pos "cannot break outside of iteration structure"
                  else ();
                  {exp = Translate.breakExp(curLabel), ty = Types.BOTTOM})

        | trexp (Absyn.ArrayExp{typ,size,init,pos}) = 
            let
            val typTy = actual_ty((case Symbol.look(tenv,typ) of SOME(ty) => ty
                        | NONE =>(ErrorMsg.error pos ("Array type undefined " ^ Symbol.name(typ) ^ " ."); Types.BOTTOM)),pos)
            val arrElType = (case typTy of Types.ARRAY(elTy,_) => actual_ty(elTy,pos)
                                            | somethingElse => (ErrorMsg.error pos ("Expected an array type but got a " ^ Types.toString(somethingElse)); Types.BOTTOM))
            val initExpTy = trexp(init)          
            val initTy = actual_ty(#ty (initExpTy), pos)
            val sizeTy = trexp (size)
            in
            if checkInt(sizeTy,pos)
            then  
                if (Types.compare(arrElType,initTy) = Types.EQ)  orelse (Types.compare(initTy,arrElType) = Types.SUBTYPE)
                then {exp = Translate.arrayExp(#exp sizeTy, #exp initExpTy) , ty = typTy}
                else(ErrorMsg.error pos ("Expected init expression to have type " ^ Types.toString(arrElType) ^ " but got init expression of type " ^ Types.toString(initTy) ^"."); {exp = Translate.nilExp(), ty = Types.BOTTOM})
            else (ErrorMsg.error pos ("Expected an int for size but got a " ^ Types.toString(#ty sizeTy) ^ "."); {exp = Translate.nilExp(), ty = Types.BOTTOM})
            end 
        |trexp (Absyn.LetExp{decs, body, pos}) =
            let 
            fun transDecs(venv,tenv,decs) = 
                let fun helper(dec, {venv,tenv,exps}) = transDec(venv,tenv,dec,level,curLabel,exps) 
                in
                foldl helper {venv=venv,tenv=tenv, exps= []} decs
                end
            val {venv=venv', tenv=tenv', exps = exps'} = transDecs(venv, tenv, decs)
            val bodyExpTy = transExp(venv', tenv', body,level, curLabel)
            in 	
            {exp = Translate.letExp(exps', #exp bodyExpTy), ty = #ty bodyExpTy}
            end
      and trvar (Absyn.SimpleVar(id, pos)) =
        (case Symbol.look(venv, id)
            of SOME(Env.VarEntry{ty,indexVar,access}) =>   {exp = Translate.simpleVar(access,level), ty = actual_ty (ty,pos)}
              | SOME(Env.FunEntry(_)) => (ErrorMsg.error pos ("Variable expected. " ^ Symbol.name(id) ^ " is a function");
                    {exp = Translate.nilExp(), ty = Types.BOTTOM})
              | NONE => (ErrorMsg.error pos ("Undefined variable " ^ Symbol.name id);
                        {exp = Translate.nilExp(), ty = Types.BOTTOM}))
      | trvar (Absyn.FieldVar(v, id, pos)) =
          let
              val {exp=vExp, ty=parentTy} = trvar(v)  
              fun find((cur,curRef)::l,index ) = (if Symbol.name cur = Symbol.name id then {exp = Translate.fieldVar(vExp ,index), ty = actual_ty(curRef,pos)} else find(l,index+1))
              | find ([],index) = (ErrorMsg.error pos ("Field " ^ Symbol.name id ^ " doesn't exist for this record."); {exp = Translate.nilExp(), ty= Types.BOTTOM})
          in
              case (parentTy) of
              Types.RECORD(fields, ref1 ) => find(fields,0)
              | notARecord => (ErrorMsg.error pos ("Trying to access a field of " ^ Types.toString(notARecord) ^ "."); {exp = Translate.nilExp(), ty = Types.BOTTOM})
          end
      | trvar(Absyn.SubscriptVar(v,id,pos)) =
        let 
          val {exp = vExp, ty = parentTy} = trvar v;
          val idTy = trexp id;
        in 
          case parentTy of Types.ARRAY(arrTy,_) => (checkInt(idTy,pos); {exp = Translate.subscriptVar(vExp, #exp idTy), ty = actual_ty(arrTy,pos)})
          | notAnArray => (ErrorMsg.error  pos ("Trying to access index of " ^ Types.toString(notAnArray) ^ "."); {exp = Translate.nilExp(), ty = Types.BOTTOM})
        end
    in 
      trexp(expression)
    end

  (*Headers then body. Check for duplicates and correct result types.*)
 and transDec(venv,tenv,Absyn.FunctionDec(funcs),level, curLabel, exps) =
      let 
        fun addHeader({name,params,result,body,pos},venv) = 
          let
            fun getParamTy{name,escape,typ,pos} = (case Symbol.look(tenv,typ) of SOME(ty) => actual_ty(ty,pos)
                        | NONE => (ErrorMsg.error pos ("Undefined type " ^ Symbol.name typ); Types.BOTTOM))

            fun getEscape{name,escape,typ,pos} = !escape
            val translatedParams = map getParamTy params
            val escapes = map getEscape params
            val resultTy = (case result of SOME(sym,pos) => (case Symbol.look(tenv,sym) of SOME (ty) => ty
                                                            | NONE => (ErrorMsg.error pos ("Result type not found in type environment"); Types.BOTTOM) )
                                          | NONE => Types.UNIT)
            val funLabel = Temp.newlabel()
            val newLevel = Translate.newLevel{parent= level, name = funLabel, formals = escapes }
            val venv' = Symbol.enter(venv,name, Env.FunEntry{formals = translatedParams, level = newLevel, label = funLabel, result = resultTy})         
         in
            venv'
          end

        fun checkDuplicates([],visited) = ()
        | checkDuplicates({name,params,result,body,pos}::l, visited ) = if contains(visited,name) then ErrorMsg.error pos ("Functions should not have the same name in recursive function declaration") else checkDuplicates(l,name::visited)

        val venv' = foldl addHeader venv funcs

        fun addBody ({name, params, result, body, pos}, {tenv, venv,exps}) = 
              let 
                  fun getParamTy{name,escape,typ,pos} = (case Symbol.look(tenv,typ) of SOME(ty) => {name = name, ty = actual_ty(ty,pos)}
                                                        | NONE => (ErrorMsg.error pos ("Undefined type " ^ Symbol.name typ); {name = name, ty = Types.BOTTOM}))

                  val translatedParams = map getParamTy params
                  val funLevel = case Symbol.look(venv,name) of SOME(Env.FunEntry{formals,level=level',label,result}) => SOME(level')
                                                          | _ =>(ErrorMsg.impossible "No FunEntry for function"; NONE)
                  val formals = case funLevel of SOME(level') => Translate.formals(level')
                            | NONE =>  (ErrorMsg.impossible "No FunEntry for function";[])

                  fun pop([]) = []
                  | pop(a::l) =  l

                  val formals' = pop(formals)

                  fun merge([],[],ct) = []
                  | merge (a::l,a1::l1,ct) = ((a,a1)::merge(l,l1,ct+1) )
                  | merge([],l1,ct) = (ErrorMsg.impossible "There are more parameters than accesses for parameters" ; [])
                  |  merge(l,[],ct) = (ErrorMsg.impossible ("There are more accesses for parameters than parameters" ^ Int.toString(ct)) ; [])

                  val mergedParam = merge(translatedParams,formals',0)

                  fun addParam( ({name = name, ty = ty},curAccess),venv ) = Symbol.enter(venv,name, Env.VarEntry{ty = ty, indexVar = false,access = curAccess})
                  val venv' =  foldl addParam venv mergedParam


                  val bodyLevel = (case funLevel of SOME(level') => level'
                                                | NONE => level )

                  val bodyExp =  transExp (venv',tenv,body,bodyLevel, curLabel)
                  val bodyTy = actual_ty( #ty(bodyExp),pos)

                  val _ = Translate.procEntryExit({level = bodyLevel, body = #exp(bodyExp)})

                  fun getResultTy(sym,tenv) = case Symbol.look(tenv,sym) of SOME(ty) => ty
                                            | NONE =>  Types.UNIT

              in
               (
                 case result of SOME(sym,pos) => 
                  if Types.compare(actual_ty(getResultTy(sym,tenv),pos), bodyTy)  = Types.EQ  orelse Types.compare(actual_ty(getResultTy(sym,tenv),pos),bodyTy ) = Types.SUBTYPE
                  then ({tenv=tenv,venv=venv, exps = exps})
                  else 
                  (ErrorMsg.error pos ("Return type does not match body. Body Type: " ^ Types.toString(actual_ty( (bodyTy,pos))) ^ " Return Type: " ^ Types.toString(actual_ty(getResultTy(sym,tenv),pos))); {tenv=tenv,venv=venv,exps = exps})
                                | NONE => (if not(checkUnit(bodyExp,pos)) then ErrorMsg.error pos ("Return type of function with no specified return type should be a unit") else (); {tenv=tenv,venv=venv, exps=exps}))
              
              end
        val _ =  foldl addBody {tenv=tenv, venv=venv', exps = exps} funcs
      in 
      checkDuplicates(funcs,[]);
      {venv = venv', tenv = tenv, exps = exps}
      end
    | transDec(venv,tenv,Absyn.VarDec{name, escape, typ = NONE, init, pos},level, curLabel, exps) = 
      let

        val initExp = transExp(venv,tenv,init,level,curLabel)
        val expTy =  actual_ty ( #ty(initExp),pos)
        val access =  Translate.allocLocal(level)(!escape)
        val venv' = Symbol.enter(venv,name,Env.VarEntry{ty = expTy, indexVar = false, access = access})
      in
        (if expTy = Types.NIL then ErrorMsg.error pos ("Can not initialize variables with no specified type as nil") else ();
        {tenv=tenv, venv = venv', exps = exps @ [Translate.assignExp(Translate.simpleVar(access,level), #exp initExp)]  })
      end
    
    | transDec(venv,tenv,Absyn.VarDec{name,escape, typ = SOME(symb,pos1),init,pos},level,curLabel,exps) = 
      (let 
        val expectedType = (case Symbol.look(tenv,symb) of SOME(ty) => ty
				                                                | NONE => (ErrorMsg.error pos1 ("Expected type not recognized.") ; Types.BOTTOM))
        val initExp = transExp(venv,tenv,init,level,curLabel)
        val actTy = actual_ty( #ty(initExp),pos )
        val expTy = actual_ty(expectedType,pos)
        val access = Translate.allocLocal(level)(!escape)
        val venv' = Symbol.enter(venv,name,Env.VarEntry{indexVar = false, ty = actTy, access = access})
      in    
        (
        if Types.compare( actTy ,expTy ) = Types.EQ orelse Types.compare(actTy, expTy) = Types.SUBTYPE
        then ({tenv=tenv, venv = venv', exps = exps @ [Translate.assignExp(Translate.simpleVar(access,level), #exp initExp)]})
        else (ErrorMsg.error pos1 ("Type mismatch between expected and actual type in variable declaration."); {tenv=tenv, venv = venv', exps = [Translate.assignExp(Translate.simpleVar(access,level), #exp initExp)] }))
      end)
    (*Headers then body, check for cycles that dont go through an array or record and duplicates*)
    | transDec(venv,tenv,Absyn.TypeDec(typeDecs), level, curLabel, exps) = 
      let
        fun addHeader({name,ty,pos},tenv) = Symbol.enter(tenv, name, Types.NAME(name, ref NONE))
        val tenv' = foldl addHeader tenv typeDecs

        fun addBody({name:Absyn.symbol, ty: Absyn.ty, pos: Absyn.pos}, tenv) = 
          
          case Symbol.look(tenv,name) of SOME(Types.NAME(_,ref1)) => (ref1:= SOME(transTy(tenv,ty)); tenv)
          | _ => (ErrorMsg.error pos ("Invalid type Declaration"); tenv)
          
        val tenv'' = foldl addBody tenv' typeDecs

        fun isCycle(tenv,visited,cur) = 
          case !cur of SOME(Types.NAME(sym, curRef)) => ( case !curRef of SOME (Types.NAME(nextSym, nextRef )) => if contains(visited,nextSym) then true else isCycle(tenv, nextSym::visited, nextRef)
                                                                          | _ => false)
          | _ => false
          
        fun checkDuplicates([],visited) = ()
        | checkDuplicates({name,ty,pos}::l, visited ) = if contains(visited,name) then ErrorMsg.error pos ("Types should not have the same name in recursive type declaration") else checkDuplicates(l,name::visited)

        fun checkCycle({name:Absyn.symbol,ty:Absyn.ty,pos:Absyn.pos}::l,tenv'') = 
          (case Symbol.look(tenv'',name) of SOME(Types.NAME(_,ref1)) => if isCycle(tenv'',[name],ref1) then ErrorMsg.error pos ("Cyclic type declarations have to pass through an array or record") else checkCycle(l,tenv'')
                                        | _ => ())
        | checkCycle ([],tenv'') = ()
      in 
        checkDuplicates(typeDecs,[]);
        checkCycle(typeDecs,tenv'');
        {venv = venv, tenv = tenv'', exps = exps}
      end
    and transTy (tenv, ty): Types.ty  =      
    case ty of
        Absyn.NameTy(sym, pos) => (case Symbol.look(tenv,sym) of SOME(ty) => ty
                            | NONE => (ErrorMsg.error pos ("The type " ^ Symbol.name sym ^ " is undefined");  Types.BOTTOM))     
        | Absyn.RecordTy(fields) =>(
            let
            fun fieldToTy({name,escape,typ,pos}) = 
                let 
                val ty = case Symbol.look(tenv,typ) of SOME(ty) => ty
                    | NONE => (ErrorMsg.error pos ("The type " ^ Symbol.name(typ) ^ " is undefined");  Types.BOTTOM)
                in 
                (name,ty)
                end
            val fieldTypes = map fieldToTy fields
            in
            Types.RECORD(fieldTypes, ref ())
            end
        )
        | Absyn.ArrayTy(sym, pos) =>(
        let 
            val arrTy = case Symbol.look(tenv,sym) of SOME(ty) =>ty
                    | NONE => (ErrorMsg.error pos ("The type " ^ Symbol.name(sym) ^ "is undefined");  Types.BOTTOM)
        in
            Types.ARRAY(arrTy , ref ())
        end
        )
  fun transProg(ast) =
          let
              val _ = Translate.reinitializeFrags()
              val progLabel = Temp.namedlabel("tig_main")
              val progLevel = Translate.newLevel{parent = Translate.outermost, name = progLabel, formals = []}
              val exp = #exp (transExp(Env.base_venv, Env.base_tenv,ast,progLevel, progLabel));
              val _   = Translate.procEntryExit({level = progLevel, body = exp})
              val result = Translate.getResult();

          in
            result
          end

end

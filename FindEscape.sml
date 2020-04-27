structure FindEscape: sig val findEscape: Absyn.exp -> unit
end =
struct
    type depth = int
    type escEnv = (depth * bool ref) Symbol.table

    fun traverseVar(env:escEnv, d:depth, s:Absyn.var): unit = 
        case s of 
        Absyn.SimpleVar(symbol,pos) => (case Symbol.look(env,symbol) of 
                                        SOME(d',escRef) => if d>d' then (escRef:= true; ()) else ()
                                        | NONE => ())
        | Absyn.FieldVar(var,symbol,pos) =>traverseVar(env,d,var)
        | Absyn.SubscriptVar(var,exp,pos) => traverseVar(env,d,var)
    and traverseExp(env:escEnv, d:depth, s:Absyn.exp): unit = 
        case s of 
        Absyn.VarExp(var) => traverseVar(env,d,var)
        | Absyn.NilExp => ()
        | Absyn.IntExp(int) => ()
        | Absyn.StringExp(string) => ()
        | Absyn.CallExp{func,args,pos} =>
            let
               fun help(arg) = traverseExp(env,d,arg)
            in
              (map help args; ())
            end
        | Absyn.OpExp{left,oper,right,pos} => (traverseExp(env,d,left);traverseExp(env,d,right);())
        | Absyn.RecordExp{fields,typ,pos} => 
            let
               fun help(sym,exp,pos) = traverseExp(env,d,exp)
            in
              (map help fields; ())
            end   
        | Absyn.SeqExp(exps) =>
            let 
                fun help(exp,pos) =  traverseExp(env,d,exp)
            in
                (map help exps; ())
            end
        | Absyn.AssignExp{var,exp,pos} => (traverseVar(env,d,var); traverseExp(env,d,exp))
        | Absyn.IfExp{test,then',else' = NONE,pos} => (traverseExp(env,d,test); traverseExp(env,d,then'))
        | Absyn.IfExp{test,then',else' = SOME(exp),pos} => (traverseExp(env,d,test); traverseExp(env,d,then'); traverseExp(env,d,exp))
        | Absyn.WhileExp{test,body,pos} => (traverseExp(env,d,test);  traverseExp(env,d,body))
        | Absyn.ForExp{var,escape,lo,hi,body,pos} =>  
            let
                val env' = Symbol.enter(env,var,(d,escape))
            in 
                traverseExp(env',d,lo); traverseExp(env',d,hi); traverseExp(env',d,body)
            end
        | Absyn.BreakExp(pos) => ()
        | Absyn.LetExp{decs,body,pos} => 
            let 
                val env' = traverseDecs(env,d,decs) 
            in
                traverseExp(env,d,body)
            end
        | Absyn.ArrayExp{typ,size,init,pos} => (traverseExp(env,d,size); traverseExp(env,d,init); ())
            
     and traverseDecs(env:escEnv, d:depth, s: Absyn.dec list): escEnv = 
        let
            fun travDec(Absyn.FunctionDec(fundecs),env) = 
                let 
                    fun travFundec({name,params,result,body,pos}) = 
                        let
                            fun addParam({name,escape,typ,pos},env) = (Symbol.enter(env,name,(d+1,escape)  ))
                            val env' = foldl addParam env params
                        in
                            traverseExp(env',d+1,body)
                        end
                in  
                    (map travFundec fundecs; env)
                end
            | travDec(Absyn.VarDec{name,escape,typ,init,pos},env) = 
                let 
                    val env' = Symbol.enter(env,name,(d,escape))
                
                in
                    (traverseExp(env',d,init); env')
                end
            | travDec(Absyn.TypeDec(typedecs),env) = env
        in
          foldl travDec env s
        end
    fun findEscape(prog: Absyn.exp) : unit = traverseExp(Symbol.empty,0,prog)
end
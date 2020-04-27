signature ENV = 
sig
	type access
	datatype enventry = VarEntry of {ty: Types.ty, indexVar:bool, access:Translate.access}
					| FunEntry of {level: Translate.level, label: Temp.label, formals: Types.ty list, result: Types.ty}
	val base_tenv : Types.ty Symbol.table
	val base_venv : enventry Symbol.table
end

structure Env :> ENV =
struct
	type access = Translate.access

	fun addData( (sym,entry), curEnv) = Symbol.enter(curEnv, Symbol.symbol(sym), entry)

	datatype enventry = VarEntry of {ty: Types.ty, indexVar: bool, access:access}
					| FunEntry of {level: Translate.level, label: Temp.label, formals: Types.ty list, result: Types.ty}

	val base_tenv = 
		let 
			val data =  [("int", Types.INT), ("string", Types.STRING)]
		in
			foldl addData Symbol.empty data 
		end

	val base_venv =
		let
			val functions = [
							("print", FunEntry{level= Translate.outermost, label = Temp.namedlabel("tig_print"), formals = [Types.STRING], result = Types.UNIT}),
							("flush", FunEntry{level= Translate.outermost, label = Temp.namedlabel("tig_flush"), formals = [], result = Types.UNIT}),
                            ("getchar", FunEntry{level= Translate.outermost, label = Temp.namedlabel("tig_getchar"), formals = [] , result = Types.STRING}),
                            ("ord", FunEntry{level= Translate.outermost, label = Temp.namedlabel("tig_ord"), formals = [Types.STRING], result = Types.INT}),
                            ("chr", FunEntry{level= Translate.outermost, label = Temp.namedlabel("tig_chr"), formals = [Types.INT], result = Types.STRING}),
                            ("size", FunEntry{level= Translate.outermost, label = Temp.namedlabel("tig_size"), formals = [Types.STRING], result = Types.INT}),
                            ("substring", FunEntry{level= Translate.outermost, label = Temp.namedlabel("tig_substring"), formals = [Types.STRING, Types.INT, Types.INT], result = Types.STRING}),
                            ("concat", FunEntry{level= Translate.outermost, label = Temp.namedlabel("tig_concat"), formals = [Types.STRING, Types.STRING], result = Types.STRING}),
                            ("not", FunEntry{level= Translate.outermost, label = Temp.namedlabel("tig_not"), formals = [Types.INT], result = Types.INT}),
                            ("exit", FunEntry{level= Translate.outermost, label = Temp.namedlabel("tig_exit"), formals = [Types.INT], result = Types.UNIT})]
		in
			foldl addData Symbol.empty functions
		end
end
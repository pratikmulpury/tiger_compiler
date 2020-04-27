structure Flow =
struct
      structure Graph = FuncGraph(struct type ord_key = int val compare = Int.compare end)
      structure NodeMap = SplayMapFn(struct type ord_key = int val compare = Int.compare end)
      
      datatype flowgraph = FGRAPH of {control: Assem.instr Graph.graph,
				    def: Temp.temp list NodeMap.map,
				    use: Temp.temp list NodeMap.map,
				    ismove: bool NodeMap.map}

  (* Note:  any "use" within the block is assumed to be BEFORE a "def" 
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).

     For any node in the graph,  
           Graph.Table.look(def,node) = SOME(def-list)
           Graph.Table.look(use,node) = SOME(use-list)
   *)

end

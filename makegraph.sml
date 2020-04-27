structure MakeGraph:
sig 
    val instrs2graph: Assem.instr list ->
                Flow.flowgraph * Assem.instr Flow.Graph.node list
end = 
struct

    structure LabelMap = SplayMapFn(struct type ord_key = string val compare = String.compare end)
    fun instrs2graph(instrs: Assem.instr list) =
        let
            val nodeNum = ref 0

            fun addNode(instr, (flowgraphNoEdges,labelMap)) = 
                let
                    val _ = nodeNum:= !nodeNum+1
                    val labelMap' = (case instr of Assem.OPER{assem,dst,src,jump} => labelMap
                                    | Assem.LABEL{assem,lab} => LabelMap.insert(labelMap, Symbol.name(lab), !nodeNum)
                                    | Assem.MOVE{assem,dst,src} => labelMap
                                    )
                in
                    (Flow.Graph.addNode(flowgraphNoEdges, !nodeNum, instr), labelMap')
                end
            
            
            val (flowgraphNoEdges,labelMap) = foldl addNode (Flow.Graph.empty, LabelMap.empty) instrs


            fun addDefs(nodes) = 
                let
                    fun addDef(node,def) = 

                        let
                            val instr = Flow.Graph.nodeInfo(node)
                        in 
                            case instr of Assem.OPER{assem , dst , src , jump } =>  Flow.NodeMap.insert(def,Flow.Graph.getNodeID(node),dst)
                            | Assem.LABEL{assem, lab}  => Flow.NodeMap.insert(def,Flow.Graph.getNodeID(node),[])
                            | Assem.MOVE{assem, dst, src} => Flow.NodeMap.insert(def,Flow.Graph.getNodeID(node), [dst])               
                        end
                  
                in
                    foldl addDef Flow.NodeMap.empty nodes
                end

            val def = addDefs(Flow.Graph.nodes(flowgraphNoEdges))

            fun addUses(nodes) = 
                let
                    fun addUse(node,use) =  
                        let
                            val instr = Flow.Graph.nodeInfo(node)
                        in
                            case instr of Assem.OPER{assem, dst, src, jump} => Flow.NodeMap.insert(use,Flow.Graph.getNodeID(node),src)
                            | Assem.LABEL{assem , lab }  => Flow.NodeMap.insert(use,Flow.Graph.getNodeID(node),[])
                            | Assem.MOVE{assem, dst, src} => Flow.NodeMap.insert(use,Flow.Graph.getNodeID(node), [src])   
                        end            
                in
                    foldl addUse Flow.NodeMap.empty nodes
                end

            val use = addUses(Flow.Graph.nodes(flowgraphNoEdges))

            fun addIsMoves(nodes) = 
                let

                    fun addIsMove(node,ismove) =  
                        let
                            val instr = Flow.Graph.nodeInfo(node)
                        in
                            case instr of Assem.OPER{assem , dst , src, jump } =>  Flow.NodeMap.insert(ismove,Flow.Graph.getNodeID(node),false)
                            | Assem.LABEL{assem  , lab }  =>  Flow.NodeMap.insert(ismove,Flow.Graph.getNodeID(node),false)
                            | Assem.MOVE{assem , dst, src } => Flow.NodeMap.insert(ismove,Flow.Graph.getNodeID(node), true)  
                        end             
                  
                in
                    foldl addIsMove Flow.NodeMap.empty nodes
                end

            val ismove = addIsMoves(Flow.Graph.nodes(flowgraphNoEdges))

            fun addEdges(instrs,flowgraph,labelMap) = 
                let
                    val index = ref 0

                    fun addJumpEdge(flowgraph, labelMap, [],cur) = flowgraph
                    | addJumpEdge(flowgraph, labelMap, a::l, cur) =  
                        
                        (case LabelMap.find(labelMap, Symbol.name(a)) of NONE => addJumpEdge(flowgraph, labelMap, l, cur)
                                                                        | SOME(ind) => addJumpEdge(Flow.Graph.addEdge(flowgraph, {from = cur, to = ind}), labelMap,l,cur) 
                        )

                         

                    fun addFallThroughEdge(flowgraph, cur) = 
                        if cur+1 > !nodeNum
                        then flowgraph
                        else Flow.Graph.addEdge(flowgraph, {from = cur, to = cur+1})

                    fun addEdge(instr, flowgraph) = 
                        ( 
                            index:= !index+1;
                            case instr of Assem.OPER{assem, src, dst, jump} => (case jump of NONE => addFallThroughEdge(flowgraph, !index)
                                                                                | SOME([]) => flowgraph 
                                                                                | SOME(l) => addJumpEdge(flowgraph, labelMap, l,!index))
                            | Assem.LABEL{assem, lab} => addFallThroughEdge(flowgraph, !index)
                            | Assem.MOVE{assem, src, dst} => addFallThroughEdge(flowgraph, !index)
                        )                        
                in
                    foldl addEdge flowgraph instrs
                end

            val flowgraph = addEdges(instrs, flowgraphNoEdges, labelMap)     
          
        in
            (Flow.FGRAPH{control = flowgraph, def = def, use = use, ismove = ismove}, Flow.Graph.nodes(flowgraph))
        end


end
structure Liveness :
sig
    datatype igraph = 
        IGRAPH of {
            graph: Temp.temp Flow.Graph.graph, 
            tnode: Temp.temp -> Temp.temp Flow.Graph.node,
            gtemp: Temp.temp Flow.Graph.node -> Temp.temp, 
            moves: (Temp.temp Flow.Graph.node * Temp.temp Flow.Graph.node) list 
            }
    val interferenceGraph: 
        Flow.flowgraph -> igraph * (Temp.temp Flow.Graph.node -> Temp.temp list)
    val show: TextIO.outstream * igraph -> unit
end =
struct

    datatype igraph = 
    IGRAPH of {
        graph: Temp.temp Flow.Graph.graph, 
        tnode: Temp.temp -> Temp.temp Flow.Graph.node,
        gtemp: Temp.temp Flow.Graph.node -> Temp.temp, 
        moves: (Temp.temp Flow.Graph.node * Temp.temp Flow.Graph.node) list 
        }


    fun interferenceGraph(Flow.FGRAPH{control,def,use,ismove}) = 

        let
            val nodes = Flow.Graph.nodes(control)
            (*Should Probably Use DFS to order the nodes at some point, just going to use reverse order in terms of location of code for now*)

            val initialLiveIn  = foldr (fn(node,map) => Flow.NodeMap.insert(map, Flow.Graph.getNodeID(node), Temp.Set.empty)) Flow.NodeMap.empty nodes
            val initialLiveOut = foldr (fn(node,map) => Flow.NodeMap.insert(map, Flow.Graph.getNodeID(node), Temp.Set.empty)) Flow.NodeMap.empty nodes

            fun livenessCalculation(initialLiveIn, initialLiveOut) = 
                let  
                    val changed = ref false
                    fun livenessOneNode(node, (liveIn, liveOut) )= 
                        let

                            fun getFromMap(map) = (case Flow.NodeMap.find(map, Flow.Graph.getNodeID(node)) of SOME(thing)=> thing
                                                                                                                | NONE => [] )

                            fun getSetFromMap(map) = (case Flow.NodeMap.find(map, Flow.Graph.getNodeID(node)) of SOME(thing)=> thing
                                                                                                                | NONE =>(Temp.Set.empty))

                            fun getFromMapTwo(map,succ) = (case Flow.NodeMap.find(map, succ) of SOME(thing)=> thing
                                                                                                                | NONE => (Temp.Set.empty) )
                            fun calcLiveIn() = 
                                let
                                    val useSet = Temp.Set.addList(Temp.Set.empty, getFromMap(use))
                                    val oldOutSet = getSetFromMap(liveOut)
                                    val defSet = Temp.Set.addList(Temp.Set.empty, getFromMap(def))
                                    val newLiveInSet = Temp.Set.union(useSet, Temp.Set.difference(oldOutSet, defSet))
                                in
                                    newLiveInSet
                                end         
                            val oldOutSet = getSetFromMap(liveOut)

                            fun calcLiveOut() =
                                let 
                                    val succs = Flow.Graph.succs(node)
                                    fun unify(suc, acc) = Temp.Set.union(acc, getFromMapTwo(liveIn,suc))
                                in
                                    foldl unify oldOutSet succs 
                                end 

                            val newInSet = calcLiveIn()
                            val newOutSet = calcLiveOut()

                            val oldInSet = getSetFromMap(liveIn)

                            val liveInNew =  Flow.NodeMap.insert(liveIn, Flow.Graph.getNodeID(node), newInSet)
                            val liveOutNew = Flow.NodeMap.insert(liveOut, Flow.Graph.getNodeID(node), newOutSet )

                            val _ =     (
                                            if Temp.Set.equal(newInSet, oldInSet)
                                            then if Temp.Set.equal(newOutSet, oldOutSet)
                                                then ()
                                                else changed:= true
                                            else  changed:= true
                                        )

                        in
                          (liveInNew, liveOutNew)
                        end
                    val (endLiveIn, endLiveOut) = foldr livenessOneNode (initialLiveIn, initialLiveOut)  nodes
                in
                   if !changed = false
                   then (initialLiveIn,initialLiveOut)
                   else livenessCalculation(endLiveIn, endLiveOut)
                end
            
            val (liveIn, liveOut) = livenessCalculation(initialLiveIn, initialLiveOut)

        
            fun addNodes(controlNode, graph) = 
                let

                    fun getFromMap(map) = (case Flow.NodeMap.find(map, Flow.Graph.getNodeID(controlNode)) of SOME(thing)=> thing
                                                                                                                | NONE => [] )
                    val defList = getFromMap(def)
                    val useList = getFromMap(use)                      

                    val graph' =  foldl (fn(temp,graph) => Flow.Graph.addNode(graph, temp, temp)) graph defList 
                    val graph'' = foldl (fn(temp,graph) => Flow.Graph.addNode(graph, temp, temp))  graph' useList
                in
                    graph''
                end

            val graphNodesOnly = foldl addNodes Flow.Graph.empty nodes
 

            fun addEdges(controlNode, (graph,liveOut)) = 
                let
                    fun getFromMap(map) = (case Flow.NodeMap.find(map, Flow.Graph.getNodeID(controlNode)) of SOME(thing)=> thing
                                                                                                                | NONE => [] )
            
                    fun getListFromMap(map) = (case Flow.NodeMap.find(map, Flow.Graph.getNodeID(controlNode)) of SOME(thing)=> Temp.Set.listItems(thing)
                                                                                                        | NONE => [] )

                    val defList = getFromMap(def)
                    val liveOutList = getListFromMap(liveOut)

                    fun addDef(def, graph) =
                        let
                            fun addEdge(outTemp,graph) = Flow.Graph.doubleEdge(graph, def, outTemp)
                        in
                            foldl addEdge graph liveOutList
                        end

                     val graph' = foldl addDef graph defList
                in
                    (graph', liveOut)
                end

            val (graph,liveOut2) = foldl addEdges (graphNodesOnly, liveOut) nodes

            fun getTNode(id) = Flow.Graph.getNode(graph, id)

            val tnode = getTNode

            val gTemp = fn(node) => Flow.Graph.nodeInfo(node)

            fun getMoves(tnode, control, graph) = 
                let
                    fun getMove(node, movelist) = (case Flow.Graph.nodeInfo(node) of Assem.MOVE{assem, src, dst} => (tnode src, tnode dst) :: movelist
                                                                                    | _ => movelist
                    )
                in
                    foldl getMove [] nodes
                end


            val moves = getMoves(tnode,control, graph)

            fun flowNodeToLiveOutTemps(flownode) = (case Flow.NodeMap.find(liveOut, Flow.Graph.nodeInfo(flownode)) of SOME(set) => Temp.Set.listItems(set)
                                                                                                                    | NONE => []
            )
        in
          (IGRAPH{graph = graph, tnode = tnode, gtemp = gTemp, moves = moves}, flowNodeToLiveOutTemps)
        end 


    (*Should Revise this to print to custom streams at some point but for now relying on funcgraph print function*)
    fun show(stream, igraph as IGRAPH{graph,tnode, gtemp, moves}) = 
        (
            TextIO.output(TextIO.stdOut, "Interference Graph \n");
            Flow.Graph.printGraph(fn(id,reg)  =>  "Temp: "  ^  MipsFrame.makestring(reg)) graph
        )
end
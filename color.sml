signature COLOR = 
    sig 
    structure Frame: FRAME

    type allocation = Frame.register Temp.Map.map
        
    val color: {interference: Liveness.igraph,
    spillCost: Temp.temp -> real,
    registers: Frame.register list}  -> allocation *  Temp.temp list 
end
structure Color : COLOR = 
struct
    structure Frame = MipsFrame

    structure NodeMap = SplayMapFn(struct type ord_key = int val compare = Int.compare end)
    type allocation = Frame.register Temp.Map.map

    structure NodeSet = SplaySetFn(struct type ord_key = int val compare = Int.compare end)

    structure EdgeKey = struct type ord_key = int*int
                fun compare((f1,t1), (f2,t2)) = 
                case Int.compare(f1,f2) of
                    EQUAL => Int.compare(t1,t2)
                | x => x
                end
    structure EdgeSet = SplaySetFn(EdgeKey)

    structure RegisterSet = SplaySetFn(struct type ord_key = string val compare = String.compare end)


    fun valOfEdgeSet(inp) = case inp of SOME(edgeset) => edgeset
                            | NONE => EdgeSet.empty

    fun valOfNodeSet(inp) = case inp of SOME(nodeset) => nodeset
                            | NONE => NodeSet.empty




    fun color({interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, spillCost, registers}) = 
        let

            (* Rather use side effects than pass around a lot of values :D *)
            val nodes = Flow.Graph.nodes graph
            val k = length(registers)

            val precolored = ref NodeSet.empty
            val initials = ref NodeSet.empty 
            val simplifyWorklist = ref NodeSet.empty
            val freezeWorklist = ref NodeSet.empty

            val spillWorklist = ref NodeSet.empty
            val spilledNodes = ref NodeSet.empty
            val coalescedNodes = ref NodeSet.empty
            val coloredNodes =  ref NodeSet.empty
            val selectStack = ref []

            val coalescedMoves = ref EdgeSet.empty 
            val constrainedMoves = ref EdgeSet.empty
            val frozenMoves = ref EdgeSet.empty 
            val worklistMoves = ref EdgeSet.empty
            val activeMoves = ref EdgeSet.empty

            val adjSet = ref EdgeSet.empty
            val adjList = ref NodeMap.empty
            val degree = ref NodeMap.empty
            val moveList = ref NodeMap.empty
            val alias = ref NodeMap.empty 
            val algColor = ref Frame.tempMap
            val remainingRegs = ref RegisterSet.empty

            val _ = remainingRegs:= RegisterSet.addList(!remainingRegs, registers)

            fun build() = 
                let 
                    fun populatePrecolored() = 
                        let
                            fun help(node, precolored) = 
                                case Temp.Map.find(Frame.tempMap, gtemp node) of 
                                    SOME(color) =>(algColor:= Temp.Map.insert(!algColor, gtemp node, color); NodeSet.add(precolored,Flow.Graph.getNodeID(node)))
                                    | NONE => precolored
                        in

                            foldl help NodeSet.empty nodes
                        end

                    val _ = precolored:= populatePrecolored() 

                    fun populateInitials() = 
                        let
                            fun help(node, initials) = 
                                case Temp.Map.find(Frame.tempMap, gtemp node) of
                                SOME(color) => initials
                                | NONE => NodeSet.add(initials, Flow.Graph.getNodeID(node))
                        in
                            foldl help NodeSet.empty nodes
                        end

                    val _ = initials:=populateInitials()

                    fun populateMoveList() =  
                        let 
                            fun oneNode(node, moveList) = 
                                let    
                                    fun getMovesInvolvingNode((src,dst), curSet) = 
                                        if  Flow.Graph.getNodeID(src) =  Flow.Graph.getNodeID(node)
                                        then  EdgeSet.add(curSet, (Flow.Graph.getNodeID(src), Flow.Graph.getNodeID(dst)))
                                        else if  Flow.Graph.getNodeID(dst) = Flow.Graph.getNodeID(node)
                                        then  EdgeSet.add(curSet, (Flow.Graph.getNodeID(src), Flow.Graph.getNodeID(dst)))
                                        else curSet
                                    val curNodeSet = foldl getMovesInvolvingNode EdgeSet.empty moves
                                in
                                  NodeMap.insert(moveList, Flow.Graph.getNodeID(node), curNodeSet)
                                end
                        in
                            foldl oneNode NodeMap.empty nodes
                        end 

                    val _ = moveList:= populateMoveList()

                    fun populateWorklistMove() = 
                        let
                            fun oneMove( (src,dst), curSet ) =

                            EdgeSet.add(curSet,   (Flow.Graph.getNodeID(src), Flow.Graph.getNodeID(dst))     )             
                        in
                            foldl oneMove EdgeSet.empty moves
                        end


                    val _ = worklistMoves:= populateWorklistMove()

                    fun populateDegree() =  
                        let 
                            fun oneNode(node, degree) = 
                                let    
                                    val curDegree = length(Flow.Graph.adj(node))
                                in
                                  NodeMap.insert(degree, Flow.Graph.getNodeID(node), curDegree)
                                end
                        in
                            foldl oneNode NodeMap.empty nodes
                        end 
                    
                    val _ = degree:= populateDegree()



                    (* check if in precolored *)
                    fun populateAdjList() =  
                        let 
                            fun oneNode(node, degree) = 
                                let    
                                    val curListWithPrecolored = Flow.Graph.adj(node)

                                    fun removePrecolored([], out) = out
                                    | removePrecolored(a::l, out) = 
                                        if NodeSet.member(!precolored,a) 
                                        then removePrecolored(l, out)
                                        else removePrecolored(l, a::out)

                                    val curListNoPrecolored = removePrecolored(curListWithPrecolored, [])
                                in
                                  NodeMap.insert(degree, Flow.Graph.getNodeID(node), curListNoPrecolored)
                                end
                        in
                            foldl oneNode NodeMap.empty nodes
                        end 

                    val _ = adjList:= populateAdjList()

                    fun populateAdjSet() = 
                        let
                            fun addEdge (node, adjSet) = 
                                let 
                                    fun oneEdge (adj, adjSet) = EdgeSet.add(    EdgeSet.add(adjSet, (Flow.Graph.getNodeID(node), adj) )    ,    (adj, Flow.Graph.getNodeID(node))        )
                                in 
                                    foldl oneEdge adjSet (Flow.Graph.adj(node))
                                end          
                        in
                            foldl addEdge EdgeSet.empty nodes
                        end

                    val _ = adjSet:= populateAdjSet()
                in
                    ()
                end
                
            val _ = build()

            fun makeWorklist() = 
                let 
                    fun iterThroughInitial(nodeId, (spillWorklist, freezeWorklist, simplifyWorklist, initials)) =
                        let
                            val initial' = NodeSet.delete(initials, nodeId)
                            fun MoveRelated(nodeId) = EdgeSet.numItems(EdgeSet.intersection( valOfEdgeSet(NodeMap.find(!moveList, nodeId)), EdgeSet.union(!activeMoves, !worklistMoves) )) > 0
                        in
                            if valOf(NodeMap.find(!degree, nodeId)) >= k
                            then (NodeSet.add(spillWorklist, nodeId) , freezeWorklist, simplifyWorklist, initial')
                            else if MoveRelated(nodeId)
                            then (spillWorklist, NodeSet.add(freezeWorklist, nodeId), simplifyWorklist, initial')
                            else (spillWorklist, freezeWorklist, NodeSet.add(simplifyWorklist, nodeId), initial')
                        end
                in
                    foldl iterThroughInitial (NodeSet.empty, NodeSet.empty, NodeSet.empty, !initials) (NodeSet.listItems(!initials))
                end


            val (spillWorklist', freezeWorklist', simplifyWorklist', initials') = makeWorklist()

            val _ = spillWorklist:= spillWorklist'

            val _ = freezeWorklist:= freezeWorklist'

            val _ = simplifyWorklist:= simplifyWorklist'

            val _ = initials:= initials'

            fun getAlias(temp) =
                if isSome(NodeMap.find(!alias, temp))
                then ( (*print("In if of get alias the temp is " ^ Int.toString(temp) ^ "\n");*) getAlias(valOf(NodeMap.find(!alias,temp))))
                else ( (*print("In else of get alias the temp is " ^ Int.toString(temp) ^ "\n");*) temp)

            fun repeat() = 
                let
                    fun adjacent(temp) =
                        let
                            val selectStackSet = NodeSet.addList(NodeSet.empty, !selectStack)
                        in
                           NodeSet.listItems(NodeSet.difference(NodeSet.addList(NodeSet.empty, valOf(NodeMap.find(!adjList,temp))), NodeSet.union(selectStackSet, !coalescedNodes)))
                        end

                    fun nodeMoves(temp) = EdgeSet.intersection(valOfEdgeSet(NodeMap.find(!moveList,temp)), EdgeSet.union(!activeMoves, !worklistMoves))

                    fun enableMoves(nodes) = 
                        let

                            fun enableMovesForNode(node, dummy) = 
                                let val moves = EdgeSet.listItems(nodeMoves(node))

                                fun enableMove(move, dummy) = 
                                    if EdgeSet.member(!activeMoves,move)
                                    then ( activeMoves:= EdgeSet.delete(!activeMoves,move) ; worklistMoves:= EdgeSet.add(!worklistMoves, move); [] )
                                    else []

                                in
                                    foldl enableMove [] moves
                                end
                        in
                            foldl enableMovesForNode [] nodes 
                        end    


                    (* takes in a list of temps to be decremented*)
                    fun decrementDegree(adjacentToRemove) = 
                        let
                            fun decrementSingleDegree(temp, dummy ) = 
                                let
                                    val d = valOf(NodeMap.find(!degree,temp))
                                    val _ = degree:= NodeMap.insert(!degree,temp,d-1)                  
                                    
                                    val _  = 
                                            if d = k
                                            then (
                                                enableMoves(temp::adjacent(temp));
                                                if NodeSet.member(!spillWorklist, temp)
                                                then spillWorklist:= NodeSet.delete(!spillWorklist, temp)
                                                else()
                                                ;
                                                if length(EdgeSet.listItems(nodeMoves(temp))) > 0
                                                then (freezeWorklist:= NodeSet.add(!freezeWorklist,temp)) 
                                                else (simplifyWorklist:= NodeSet.add(!simplifyWorklist,temp))
                                                    ;
                                                 [])
                                            else []
                                         
                                in
                                    []
                                end

                            val  _ = foldl decrementSingleDegree [] adjacentToRemove                    

                        in
                            ()
                        end

                    fun simplify() = 
                        let
                            val elToRemove = List.nth(NodeSet.listItems(!simplifyWorklist), 0)
                            val _ = simplifyWorklist:= NodeSet.delete(!simplifyWorklist, elToRemove)
                            val _ = selectStack:= elToRemove :: !selectStack

                            val adjacentToRemove = adjacent(elToRemove)
                            val _  =  decrementDegree(adjacentToRemove)
                        in
                            ()
                        end

                    fun ok(t,r) = (valOf(NodeMap.find(!degree, t)) < k  orelse  NodeSet.member(!precolored, t)  orelse EdgeSet.member(!adjSet, (t,r)))


                    fun conservative(nodes) = 
                        let
                            fun check(curK,[]) = curK < k
                            | check(curK, a::l) = 
                                if valOf(NodeMap.find(!degree,a)) >= k
                                then check( curK+1, l)
                                else check(curK, l)
                        in
                            check(0,nodes)
                        end

                    fun MoveRelated(nodeId) = EdgeSet.numItems(EdgeSet.intersection( valOfEdgeSet(NodeMap.find(!moveList, nodeId)), EdgeSet.union(!activeMoves, !worklistMoves) )) > 0


                    fun addEdge(u,v) = 
                                if not(EdgeSet.member(!adjSet, (u,v))) andalso (u <> v)
                                (*u,v not part of set and dont equal each other *)
                                then(
                                    adjSet:= EdgeSet.add(!adjSet, (u,v))
                                    ;
                                    if not(NodeSet.member(!precolored, u))
                                    then ( adjList:=  NodeMap.insert(!adjList,u, NodeSet.listItems(NodeSet.union( NodeSet.addList(NodeSet.empty,valOf(NodeMap.find(!adjList,u))), NodeSet.add(NodeSet.empty, v))));  
                                    degree:= NodeMap.insert(!degree, u, valOf(NodeMap.find(!degree,u))+1))
                                    else ()
                                    ;
                                    if not(NodeSet.member(!precolored, v))
                                    then  ( adjList:=  NodeMap.insert(!adjList,v, NodeSet.listItems(NodeSet.union(NodeSet.addList(NodeSet.empty,valOf(NodeMap.find(!adjList,v))), NodeSet.add(NodeSet.empty, u)))); 
                                     degree:= NodeMap.insert(!degree, v, valOf(NodeMap.find(!degree,v))+1))
                                    else ()
                                )
                                else()
        


                    fun combine(u,v) = 
                        let
                            val _ = if NodeSet.member(!freezeWorklist,v)
                                    then (freezeWorklist:= NodeSet.delete(!freezeWorklist, v))
                                    else (spillWorklist:= NodeSet.delete(!spillWorklist, v))

                            val _ = coalescedNodes:= NodeSet.add(!coalescedNodes, v)
                            val _ = alias:= NodeMap.insert(!alias, v, u)
                            val _ = moveList:= NodeMap.insert(!moveList, u,    EdgeSet.union(   valOfEdgeSet(NodeMap.find(!moveList, u)),  valOfEdgeSet(NodeMap.find(!moveList, v))  )  )

                            val adjV = adjacent(v) 

                            fun help(t, l) = ( addEdge(t,u);  decrementDegree([t]); [])


                            val _ = foldl help [] adjV
                            val _ = 
                                if valOf(NodeMap.find(!degree, u)) > k andalso NodeSet.member(!freezeWorklist, u)
                                then (freezeWorklist:= NodeSet.delete(!freezeWorklist, u) ;   spillWorklist:=  NodeSet.add(!spillWorklist, u))
                                else()
                        in
                            ()
                        end


                    fun addWorkList(temp) = 
                        if not(NodeSet.member(!precolored, temp)) andalso not(MoveRelated(temp)) andalso valOf(NodeMap.find(!degree, temp)) < k 
                        then (freezeWorklist:= NodeSet.delete(!freezeWorklist, temp ); simplifyWorklist:= NodeSet.add(!simplifyWorklist,temp)    )
                        else ()



                    fun coalesce()  = 
                        let 
                            val (x',y') = List.nth(EdgeSet.listItems(!worklistMoves), 0)
                            val m = (x',y')
                            
                            val x = getAlias(x')
                            val y = getAlias(y')

                            val (u,v) = if NodeSet.member(!precolored, y)
                                        then (y,x)
                                        else (x,y)

                            val _ =  worklistMoves:= EdgeSet.delete(!worklistMoves, m)

                            fun check([],b) = true
                            | check([a],b) = ok(a,b)
                            | check(a::l,b) = ok(a,b) andalso check(l,b)

                            val _ =
                                    if u = v
                                    then (coalescedMoves:= EdgeSet.add(!coalescedMoves, m); addWorkList(u))
                                    else if NodeSet.member(!precolored,v) orelse EdgeSet.member(!adjSet, (u,v))
                                    then (constrainedMoves:= EdgeSet.add(!constrainedMoves, m) ; addWorkList(u) ; addWorkList(v) )  
                                    else if (NodeSet.member(!precolored,u) andalso check(adjacent(u),v)) orelse ( not(NodeSet.member(!precolored,u)) andalso conservative(NodeSet.listItems(NodeSet.union(NodeSet.addList(NodeSet.empty,adjacent(u)), NodeSet.addList(NodeSet.empty, adjacent(v))))))
                                    then (coalescedMoves:= EdgeSet.add(!coalescedMoves, m); combine(u,v); addWorkList(u))
                                    else (activeMoves:= EdgeSet.add(!activeMoves, m))
                        in
                            ()
                        end

                        fun freezeMoves(u) = 
                            let
                                val nodeMovesU = nodeMoves(u) 

                                fun freezeMove((x,y), l) = 
                                    let
                                        val v = if getAlias(y) = getAlias(u)
                                                then getAlias(x)
                                                else getAlias(y)
                                        val _ =  activeMoves:= EdgeSet.delete(!activeMoves, (x,y))
                                        val _ = frozenMoves:= EdgeSet.add(!frozenMoves, (x,y))

                                        val _ = if length(EdgeSet.listItems(nodeMoves(v))) = 0 andalso valOf(NodeMap.find(!degree, v)) < k
                                                then (
                                                    if NodeSet.member(!freezeWorklist, v)
                                                    then freezeWorklist:= NodeSet.delete(!freezeWorklist, v) 
                                                    else() ;
                                                    simplifyWorklist:= NodeSet.add(!simplifyWorklist, v)
                                                )
                                                else ()
                                    in
                                        l 
                                    end
                                val _ = foldl freezeMove [] (EdgeSet.listItems(nodeMovesU))
                            in
                                ()
                            end

                        fun freeze() = 
                            let
                                val u = List.nth(NodeSet.listItems(!freezeWorklist),0)
                                val _ = freezeWorklist:= NodeSet.delete(!freezeWorklist, u)
                                val _ = simplifyWorklist:= NodeSet.add(!simplifyWorklist, u)
                                val _ = freezeMoves(u)
                            in
                                ()
                            end

                            fun selectSpill () =
                                let
                                    val spillList = NodeSet.listItems(!spillWorklist)

                                    fun findMin(temp, (curMinVal, curMin))  = 
                                        let
                                            val curTempVal= spillCost(temp)
                                            val curMin' = if Real.<(curTempVal, curMinVal)
                                                            then temp
                                                            else  curMin
                                            val curMinVal' = if Real.<(curTempVal, curMinVal)
                                                            then curTempVal
                                                            else curMinVal

                                        in
                                            (curMinVal', curMin')
                                        end


                                    val (_,min) = foldl findMin (Real.posInf,List.nth(spillList,0)) spillList

                                    val _ = spillWorklist:= NodeSet.delete(!spillWorklist, min);
                                    val _ = simplifyWorklist:=NodeSet.add(!simplifyWorklist, min);
                                    val _ = freezeMoves(min)
                				in 
                                    ()
                                end
		
                        fun performPhase() = 
                            if NodeSet.numItems(!simplifyWorklist) > 0  
                            then (simplify(); performPhase())
                            else if EdgeSet.numItems(!worklistMoves) > 0
                            then (coalesce(); performPhase())
                            else if NodeSet.numItems(!freezeWorklist) > 0
                            then (freeze(); performPhase())
                            else if NodeSet.numItems(!spillWorklist) > 0
                            then (selectSpill(); performPhase())
                            else ()

                in
                  performPhase()
                end

            val _ = repeat()

            fun assignColors() = 
                let 

                    fun doStuff() = 
                        let
                            val n = List.nth(!selectStack,0)

                            val _  = selectStack:= List.tl (!selectStack)
                            val okColors = ref RegisterSet.empty
                            val _ = okColors:= !remainingRegs
                            (*todo clean up the logic*)
                            fun help(w, l) = 

                                let
                                    val colorGetAlias = case Temp.Map.find(!algColor, getAlias(w)) of SOME(thing) => RegisterSet.add(RegisterSet.empty, thing)
                                                        | NONE => (RegisterSet.empty)

                                in
                                    if NodeSet.member( NodeSet.union(!precolored, !coloredNodes), getAlias(w))
                                    then (okColors:= RegisterSet.difference(!okColors, colorGetAlias); [])
                                    else []

                                end

                            val _ = foldl help [] (valOf(NodeMap.find(!adjList,n)))

                            val _ = 
                                    (
      
                                    if not(NodeSet.member(!precolored, n)) andalso not(NodeSet.member(!spilledNodes,n))
                                    then algColor:= Temp.Map.insert( !algColor, n, List.nth( RegisterSet.listItems(!okColors),0))
                                    else ();
                                    if  length(RegisterSet.listItems(!okColors)) = 0
                                    then spilledNodes:= NodeSet.add(!spilledNodes, n)
                                    else coloredNodes:= NodeSet.add(!coloredNodes, n)
                                    )
                        in
                            ()
                        end

                    fun loop() = 
                        if length(!selectStack) = 0
                        then ()
                        else (doStuff(); loop())

                    val _ = loop()

                    fun colorCoalescedNodes() = 
                        let
                            fun colorCoalescedNode(n, l) = 
                            
                                let
                                    val nAlias = getAlias(n)
                                    val color = valOf(Temp.Map.find(!algColor, nAlias))
                                    val _ = algColor:= Temp.Map.insert(!algColor, n, color)
                                in
                                    l
                                end

                        in  
                            foldl colorCoalescedNode [] (NodeSet.listItems(!coalescedNodes))
                        end
                    val _ = colorCoalescedNodes()
                in
                    ()
                end
            val _ = assignColors()

            (*
            fun printPrecolored(value) = print("\n The temp " ^ Frame.makestring(value) ^ " is precolored \n")
            val _ = map printPrecolored (NodeSet.listItems(!precolored))


            fun printAlias(key, value) = print("\n For Temp " ^ Frame.makestring(key) ^ "the corresponding alias is " ^ Frame.makestring(value) ^ " \n")
            val _ = map printAlias (NodeMap.listItemsi(!alias))


            fun printAlgColor(key, value) = print("\n For Temp " ^ Frame.makestring(key) ^ "the corresponding color is " ^ value ^ " \n")

            val _ = map printAlgColor (Temp.Map.listItemsi(!algColor))
            *)
        in
            (!algColor, NodeSet.listItems(!spilledNodes))
        end
end

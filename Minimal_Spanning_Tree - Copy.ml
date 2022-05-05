type graph = (int * float * int) list;;
let graph1 = [(0,7.,1);(1,3.,2);(2,2.,3);(3,1.,1)];; 
let graph2 = [(1,2.,2);(2,1.5,3);(3,4.,5);(5,7.,4);(4,3.,1)];; 
let graph3 = [(1,3.,2);(2,4.,3);(3,2.,5);(3,1.,4);(4,0.,5)];; 


(*returns lightest node to given graph*)
let lightest_node graph = 
  let rec lightest' graph node =
    match graph with
    [] -> node
    |x::xs -> let (a,b,c) = node in let (q,w,e) = x in if b > w then lightest' xs x else lightest' xs node 
in lightest' graph (List.hd graph);;

(*pops node from a given graph if it exists*)
let pop_node node graph =
  let rec pop_node' node graph result =
    match graph with 
    [] -> result
    |x::xs -> if x = node then pop_node' node xs result else pop_node' node xs (x::result)
  in pop_node' node graph [];;   

(*boolean value wether list contains element*)  
let rec is_in_list element list =
  match list with 
  [] -> false
  |x::xs -> if x = element then true else is_in_list element xs;;

(*returns array of vertexes*)
let vertexes_in_graph graph = 
  let rec vertexes graph stack = 
    match graph with 
    [] -> stack
    |x::xs -> let (a,b,c) = x in if( is_in_list a stack = false && is_in_list c stack = false  ) then  vertexes xs (c::a::stack) 
    else if( is_in_list a stack = false)  then  vertexes xs (a::stack)  else if ( is_in_list c stack = false)  then  vertexes xs (c::stack) 
    else vertexes xs stack
  in vertexes graph [];;

(*This function takes argument graph and int value of vertex and returns boolean value weather this vertex is taken by other vertex
  and have edge between or its free. it is helper of other function  called "already_visit" and is used only there*)
let  is_free_for_each graph nodenumb =
  let rec vis graph nodenumb =
    match graph with
    [] -> false
    |x::xs -> let (a,b,c) = x in if nodenumb = a || nodenumb = c then true else vis xs nodenumb
  in vis graph nodenumb;;


(*returns boolean value, if both of vertexes in node is occupied by other or not*)
let already_visited graph node =
    let (a,b,c) = node in
    is_free_for_each graph a && is_free_for_each graph c;;

(*returns list of edges contained in graph*)
let edges_in_graph graph =
  let rec edges graph result =
    match graph with 
    [] -> result
    |x::xs -> let (a,b,c) = x in edges xs (b::result)
  in edges graph [];;

(*main function*)
let msp graph =
  let rec msp' graph genGraph =
    if  graph = [] then genGraph
    else
      let ligth = lightest_node graph in 
      if (already_visited genGraph ligth) = false then 
        msp' (pop_node ligth graph) (ligth :: genGraph) 
      (*this if and else if part detects if adding the node to generated graph would create cycle or not *)
      (*logic: if it is cycle both of node should be occupied and edges of graph should be equal of vertexes of graph. | else part is if its cylce :) |*)
    else if
      List.length (edges_in_graph (ligth::genGraph)) + 1 = 
      List.length (vertexes_in_graph (ligth::genGraph)) then  
      msp' (pop_node ligth graph) (ligth::genGraph)
    else msp' (pop_node ligth graph) (genGraph)
  in msp' graph [];;
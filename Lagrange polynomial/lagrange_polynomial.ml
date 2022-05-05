let poly number list =
  (
    fun index ->
      let rec prod lst  =
        match lst with
        [] -> 1.
        |hd::tl -> let (a,b) = (List.nth list index) in let (q,w) = hd in
        if(a <> q) then ( number -. q) /. (a -. q) *. prod tl 
        else prod tl 
      in prod list 
  );;

let lagrange list = 
  (
    fun x ->
      let rec sum lst index =
      match lst with 
      [] -> 0.
      |hd::tl -> let (a,b) = hd in b *. poly x list index  +. sum tl (index + 1)
      in sum list 0
  );;
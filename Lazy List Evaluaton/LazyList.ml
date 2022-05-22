type 'a llist = Cons of 'a * (unit -> 'a llist);;
let rec lnat n  =
Cons (n,fun () -> lnat (n+1));;

let lfib unit = 
  let rec lfib' a b =
    Cons(a,fun () -> lfib' b (a+b))
  in lfib' 0 1;;

let lfact unit =
  let rec lfact' a k =
    Cons(a, fun () -> lfact' (a*k) (k + 1))
  in lfact' 1 2;;

let rec lfunc func f_node =
    Cons((func f_node), fun () -> lfunc func (f_node + 1));;

let rec lfilter predicate llist =
  let Cons(a,b) = llist in if predicate a then Cons(a,fun () -> lfilter predicate (b ()))
  else lfilter predicate (b ());;

let rec lmap func llist =
  let Cons(a,b) = llist in Cons((func a), fun () -> lmap func (b ()));;

let rec ltake n llist =
  if n = 0 then [] else 
    let Cons(a,b) = llist in a::(ltake (n-1) (b ()));;
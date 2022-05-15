type changeType = One | Five | Ten | TwentyFive | Hundered;; 
let value changeType =
  match changeType with
  One -> 1
  |Five -> 5
  |Ten -> 10
  |TwentyFive -> 25
  |Hundered -> 100;;

let fit_change changeType =
  match changeType with 
  One -> One
  |Five -> One
  |Ten -> Five
  |TwentyFive -> Ten
  |Hundered -> TwentyFive;;

let get_proper_change money = 
  let rec proper money changeType acc =
    if money < 0 then failwith "Cant have negative change"
    else
    if money = 0 then
      acc
    else 
    if money >= (value changeType) then
      proper (money - (value changeType)) changeType (changeType::acc)
    else 
      proper (money) (fit_change changeType) acc
    in proper money Hundered [];;
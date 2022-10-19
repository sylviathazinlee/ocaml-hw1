(**HW 1 Due 9/25/2022*)

(*Question 1*)
let rec pow x n = 
          if n = 0 then 1 else x * pow x (n-1);;

let rec float_pow x n = 
          if n = 0 then 1.0 else x *. float_pow x (n-1);;


(*Question 2*)
let rec compress lst = 
  match lst with
  | first:: (second:: _ as remain) -> 
    if first=second then compress remain 
    else first:: compress remain
  | origlist -> origlist;;


(*Question 3*)  
let rec remove_if lst f =
    match lst with
    | [] -> []
    | first::remain -> 
      if f(first) then remove_if remain f 
      else first:: remove_if remain f;;



(*Question 4*)
let rec slice lst i j = 
      match lst with
      | [] -> []
      | head::remain -> 
        let r = 
          if i>j then [] 
          else if j<=1 then []
          else slice remain (i-1) (j-1) in 
            if i >= 1 then r 
            else head::r;;

      (*TRACING
      ["a";"b";"c";"d";"e";"f";"g";"h"] 2 6;;
      ["b";"c";"d";"e";"f";"g";"h"] 1 5;;
      ["c";"d";"e";"f";"g";"h"] 0 4;;
      ["d";"e";"f";"g";"h"] -1 3;;
      ["e";"f";"g";"h"] -2 2;;
      ["f";"g";"h"] 2 6;; -> r becomes [] so ["f"] and go back*)



(*Question 5*)
let rec equivs f lst= 
    match lst with
    | [] -> []
    | head::tail ->
        let rec test lst_ bool=
        match lst_ with 
        | [] -> []
        | h::t ->
            if (f h head) == bool then h::test t bool
            else test t bool
          in test lst true::equivs f (test lst false);;
            


(*Question 6*)
let rec range start stop =
  if start=stop then []
  else start::(range (start+1) stop);;


let prime num =   
  let rec isPrime lst =
    match lst with
    | [] -> true
    | head::tail -> 
        if num mod head = 0 then false
        else isPrime tail 
    in isPrime (range 2 num);;


let primeLst num = 
  let x= range 3 num in 
  let rec random lst = 
    match lst with 
    | [] -> []
    | head::tail -> 
        if prime head = true then head::random tail
        else random tail
      in random x;;

let goldbachpair num = 
    let x = primeLst num in 
    let rec checkLst lst1 lst2 = 
    match lst1,lst2 with 
    | [], _ -> (0,0)
    | head::tail, head2::tail2 ->
        if head + head2 = num then (head , head2) 
        else checkLst lst1 tail2
    | head::tail, [] -> 
        checkLst tail lst2
      in checkLst x x;;
      

(*Question 7*)
let rec equiv_on f g lst =
  match lst with
  | [] -> true
  | head::remain -> 
  if f(head)=g(head) then equiv_on f g remain
  else false;;


(*Question 8*)
let rec pairwisefilter cmp lst =
  match lst with
  | [] -> []
  | first::[] -> [first]
  | first::second::remain -> let remain2= pairwisefilter cmp remain in 
       cmp first second::remain2;;
            

(*Question 9*)
let rec polynomial lst = 
    fun x -> match lst with
             | [] -> 0
             | (a,b)::remain -> let restlst = polynomial remain in
                                          let num = (pow x b) in (a* num) + restlst x;;



(*Question 10*)
let rec map f lst = 
  match lst with
  | [] -> []
  | head::remain -> f(head)::map f remain;;

let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | head::remain -> head :: append remain lst2;;
              

 let rec powerset lst = 
  match lst with
  | [] -> [[]]
  | head::remain -> let mapRest = powerset remain in 
    append mapRest (map (fun x -> head::x) mapRest);;

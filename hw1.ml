(**HW 1 Due 9/25/2022*)

(*Question 1*)
let rec pow x n= if n = 0 then 1 else x * pow x (n-1);;

print_int (pow 2 3);;
let rec float_pow x n= if n=0 then 1.0 else x *. float_pow x (n-1);;

print_string("\n");;
print_float (float_pow 3.5 0);;

(*Question 2*)
let rec compress lst = 
  match lst with
  | first:: (second:: _ as remain) -> 
    if first=second then compress remain 
    else first:: compress remain
  | origlist ->origlist;;

(*Question 3*)  
let rec remove_if lst f=
    match lst with
    | first::remain -> 
      if f(first) then remove_if remain f 
      else first:: remove_if remain f
    | [] -> [];;

(*Question 4*)
let rec slice lst i j= 
      match lst with
      | [] -> []
      | head::remain -> 
        let r = 
          if i>j then [] 
          else if j<=1 then []
          else slice remain (i-1) (j-1) in 
            if i >= 1 then r 
            else head::r;;

(*Question 5*)




    
 




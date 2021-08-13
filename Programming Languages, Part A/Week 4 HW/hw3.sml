(* Score: 95/100 *)

(* Provided Code *)

exception NoAnswer
	
(* Problem 1
Write a function only_capitals that takes a string list and returns a string list that has only the strings in the argument that start with an uppercase letter. 
Assume all strings have at least 1 character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution.
*)
fun only_capitals (los : string list) =
    (* (string list) -> (string list) *)
    (* filters list for only string that start with capital letters *)
    (* ASSUME: all string have at least one character *)
    List.filter (fn s => Char.isUpper(String.sub(s,0))) los


(* Problem 2
Write a function longest_string1 that takes a string list and returns the longest string in the list. 
If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the list. 
Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive).
*)
fun longest_string1 (los : string list) =
    (* (string list) -> (string list)|"" *) 
    (* produces longest string in list; if empty, return "" *)
    (* TIE EXCEPTION: return string closest to beginning of list *)
    List.foldl (fn (x,y) => if String.size x > String.size y then x else y)
	       "" los


(* Problem 3
Write a function longest_string2 that is exactly like longest_string1 except in the case of ties it returns the string closest to the end of the list. 
Your solution should be almost an exact copy of longest_string1. Still use foldl and String.size.
*)
fun longest_string2 (los : string list) =
    (* (string list) -> (string list)|"" *)
    (* produces longest string in list; if empty, return "" *)
    (* TIE EXCEPTION: return string closest to end of list *)
    List.foldl (fn (x,y) => if String.size x >= String.size y then x else y)
	       "" los


(* Problem 4 *)

(* Part A
longest_string_helper has type (int * int -> bool) -> string list -> string
(notice the currying). This function will look a lot like longest_string1 and longest_string2 but is more general because it takes a function as an argument.
If longest_string_helper is passed a function that behaves like > (so it returns true exactly when its first argument is stricly greater than its second), 
then the function returned has the same behavior as longest_string1.
*)
fun longest_string_helper f (los : string list) =
    (* (int * int -> bool) (string list) -> string *)
    (* helper function that produces longest string in list; if empty, return "" *)
    List.foldl (fn (x,y) => if f(String.size(x),String.size(y)) then x else y) "" los


(* Part B
longest_string3 has the same behavior as longest_string1 and longest_string4 has the same behavior as longest_string2.
longest_string3 and longest_string4 are defined with val-bindings and partial applications of longest_string_helper.
*)
val longest_string3 = longest_string_helper (fn (x,y) => x > y)
    (* function is identical to longest_string1 *)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)
    (* function is identical to longest_string2 *)


(* Problem 5
Write a function longest_capitalized that takes a string list and returns the longest string in the list that begins with an uppercase letter, 
or "" if there are no such strings. Assume all strings have at least 1 character. 
Use a val-binding and the ML library’s o operator for composing functions. Resolve ties like in problem 2.
*)
val longest_capitalized = longest_string3 o only_capitals
    (* (string list) -> string *)
    (* produce the longest string that begins with an uppercase letter, otherwise "" *)
    (* ASSUME: all strings have at least one character *)
    (* TIE EXCEPTION: return string closest to beginning of list *)

						
(* Problem 6
Write a function rev_string that takes a string and returns the string that is the same characters in reverse order. 
Use ML’s o operator, the library function rev for reversing lists, and two library functions in the String module.
*)
fun rev_string (s : string) =
    (* string -> string *)
    (* produce same string but in reverse order *)
    (String.implode o List.rev o String.explode) s


(* Problem 7
Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 argu- ments are curried). 
The first argument should be applied to elements of the second argument in order until the first time it returns SOME v for some v 
and then v is the result of the call to first_answer. 
If the first argument returns NONE for all list elements, then first_answer should raise the exception NoAnswer.
*)
fun first_answer f lst =
    (* ('a -> 'b option) ('a list) -> 'b *)
    (* apply function to each element in list until v is returned, otherwise raise exception *)
    case lst of
	[] => raise NoAnswer
      | l::lst' => case f l of
		       SOME v => v
		     | NONE => first_answer f lst'


(* Problem 8
Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option (notice the 2 arguments are curried). 
The first argument should be applied to elements of the second argument. If it returns NONE for any element, then the result for all_answers is NONE. 
Else the calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn 
and the result of all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn’t matter).
*)
fun all_answers f lst =
    (* ('a -> 'b option) ('a list) -> ('b list option) *)
    (* apply function to each element in list and produces SOME lst, otherwise NONE *)
    let fun aux(lst,acc) =
	    case lst of
		[] => SOME acc
	      | l::lst' => case f l of
			       SOME v => aux(lst',v @ acc)
			     | NONE => NONE
    in
	aux(lst,[])
    end
	

(* Provided Code *)

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end	

(* Problem 9 *)

(* Part A
Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard patterns it contains.
*)
fun count_wildcards (p : pattern) =
    (* pattern -> int *)
    (* produce the number of Wildcards in a pattern *)
    case p of
	 Wildcard => 1
       | TupleP ps => (g (fn () => 1) (fn x => 0) p)
       | _ => 0
	  
   
(* Part B
Use g to define a function count_wild_and_variable_lengths that takes a pattern 
nd returns the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables in the variable patterns it contains. 
(Use String.size. We care only about variable names; the constructor names are not relevant.)
*)
fun count_wild_and_variable_lengths (p : pattern) =
    (* pattern -> int *)
    (* produce number of Wildcards patterns and sum of string lengths of all 
       variables in Variable patterns *)
    let fun var_lengths p =
	    case p of
		Variable s => String.size(s)
	      | TupleP ps => (g (fn () => 0) (fn x => String.size(x)) p)
	      | ConstructorP(_,p) =>  (g (fn () => 0) (fn x => String.size(x)) p)
	      | _ => 0
      in
	  count_wildcards(p) + var_lengths(p)
      end

(* Part C
Use g to define a function count_some_var that takes a string and a pattern (as a pair) 
and returns the number of times the string appears as a variable in the pattern. We care only about variable names; the constructor names are not relevant.
*)
fun count_some_var (s : string, p : pattern) =
    (* string pattern -> int *)
    (* produce the number of times a string appears as a variable in the
       pattern *)
    let fun same_string_counter s1 s2 =
	    if s1 = s2
	    then 1
	    else 0
	fun count_some_var0 p =
	    case p of
		Variable s0 => same_string_counter s s0
	      | TupleP ps => (g (fn () => 0) (fn x => same_string_counter s x) p)
	      | ConstructorP(_,p) =>  (g (fn () => 0) (fn x => same_string_counter s x) p)
	      | _ => 0
					
    in
	count_some_var0 p
    end


(* Problem 10
Write a function check_pat that takes a pattern and returns true if 
and only if all the variables appearing in the pattern are distinct from each other (i.e., use different strings). 
The constructor names are not relevant. Hints: The sample solution uses two helper functions. 
The first takes a pattern and returns a list of all the strings it uses for variables. Using foldl with a function that uses @ is useful in one case. 
The second takes a list of strings and decides if it has repeats
*)
fun check_pat (p : pattern) =
    (* pattern -> bool *)
    (* produce true iff all variables are unique *)
    let fun get_var_list p =
	    case p of
		Variable s => [s]
	      | TupleP ps => List.foldl (fn (x,y) => case x of
							 Variable str => [str] @ y
						       | _ => []) [] ps
	      | ConstructorP(_,p) => get_var_list p
	      | _ => []
	fun no_repeats los acc =
	    case los of
		[] => true
	      | s::los' => if List.exists (fn x => s = x) acc
			   then false
			   else no_repeats los' ([s] @ acc)
    in
	no_repeats (get_var_list p) []
    end


(* Problem 11
Write a function match that takes a valu * pattern and returns a (string * valu) list option, 
namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does. 
Note that if the value matches but the pattern has no patterns of the form Variable s, then the result is SOME []. 
Hints: Sample solution has one case expression with 7 branches. The branch for tuples uses all_answers and ListPair.zip.
*)
fun match (v : valu, p : pattern) =
    (* valu pattern -> (string valu) list option *)
    (* produce SOME lst for patterns that match, otherwise NONE *)
    case (v,p) of
	(v, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const n1, ConstP n2) => if n1 = n2
				 then SOME []
				 else NONE
      | (Tuple vs, TupleP ps) => if List.length(vs) = List.length(ps)
				 then all_answers match (ListPair.zip(vs,ps))
				 else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1 = s2 andalso (case match(v,p) of
								       SOME _ => true
								     | NONE => false)
						   then match(v,p)
						   else NONE
      | (v, _) => NONE


(* Problem 12
Write a function first_match that takes a value and a list of patterns and returns a
(string * valu) list option, namely NONE if no pattern in the list matches 
or SOME lst where lst is the list of bindings for the first pattern in the list that matches. 
Use first_answer and a handle-expression.
*)
fun first_match (v : valu) (lop : pattern list) =
    (* valu (pattern list) -> (string valu) list option *)
    (* produces SOME lst where lst is binding for first pattern in list that matches, otherwise NONE *)
    SOME (first_answer (fn x => match(v,x)) lop)
    handle NoAnswer => NONE

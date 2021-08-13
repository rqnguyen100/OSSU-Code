(* Score: 97.5/100 *)

(* Provided Code *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Problem 1 *)

(* Part A
Write a function all_except_option, which takes a string and a string list. 
Return NONE if the string is not in the list, else return SOME lst where lst is identical to the argument list except the string is not in it. 
You may assume the string is in the list at most once. Use same_string, provided to you, to compare strings.
*)
fun all_except_option (s : string, los : string list) =
    (* String (listof String) -> NONE|(listof String) *)
    (* Return NONE if string is not in list, otherwise
       produce a list with the string removed *)
    (* ASSUME: string is in list at most once *)
    let fun aux(los,acc) =
	    case los of
		[] => NONE
	      | x::los' => if same_string(x,s)
			   then SOME (acc @ los')
			   else aux(los',x::acc)
    in
	aux(los,[])
    end


(* Part B
Write a function get_substitutions1, which takes a string list list (a list of list of strings, the substitutions) and a string s and returns a string list. 
The result has all the strings that are in some list in substitutions that also has s, but s itself should not be in the result.
Assume each list in substitutions has no repeats. The result will have repeats if s and another string are both in more than one list in substitutions.
*)
fun get_substitutions1 (llos : (string list) list, s : string) =
    (* (listof (listof String)) String -> (listof String) *)
    (* produce a list of strings that has the string in one of the strings
       but not the actual string itself *)
    (* ASSUME: each list has no repeat strings *)
    case llos of
	[] => []
      | los::llos' => let val option = all_except_option(s,los)
		      in
			  case option of
			      NONE => get_substitutions1(llos',s)
			    | SOME x => x @ get_substitutions1(llos',s)
		      end


(* Part C
Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive local helper function.
*)
fun get_substitutions2 (llos : (string list) list, s : string) =
    (* (listof (listof String)) String -> (listof String) *)
    (* produce a list of strings that has the string in one of the strings
       but not the actual string itself *)
    (* ASSUME: each list has no repeat strings *)
    let fun aux(llos,acc) =
	    case llos of 
		[] => acc
	      | los::llos' => let val option = all_except_option(s,los)
			      in
				  case option of
				      NONE => aux(llos',acc)
				   |  SOME x => aux(llos', acc @ x)
			      end
    in
	aux(llos,[])
    end

	
(* Part D
Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and (c)) 
and a full name of type {first:string,middle:string,last:string} and returns a list of full names (type {first:string,middle:string,last:string} list). 
The result is all the full names you can produce by substituting for the first name (and only the first name) using substitutions and parts (b) or (c). 
The answer should begin with the original name (then have 0 or more other names). Do not eliminate duplicates from the answer.
*)
fun similar_names (llos : (string list) list, full_name : {first:string,
		   middle:string,last:string}) =
    (* (listof (listof String)) {String, String, String} -> (listof {String,
       String, String}) *)
    (* produce list of all full names by substituting for the first name *)
    let val {first=x,middle=y,last=z} = full_name
	fun aux(los, acc) =
	    case los of
		 [] => acc
	       | s::los' => aux(los', acc @ [{first=s,middle=y,last=z}])
	    
    in
	aux(get_substitutions2(llos,x),[full_name])
    end


(* ----------------- *)
	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* Problem 2 *)

(* Part A
Write a function card_color, which takes a card and returns its color (spades and clubs are black, diamonds and hearts are red)
*)
fun card_color (c : card) =
    (* Card -> Color *)
    (* produce color of card *)
    let val (s,r) = c
    in
	case s of
	    Clubs => Black
	  | Spades => Black
	  | _  => Red
    end
			       

(* Part B
Write a function card_value, which takes a card and returns its value (numbered cards have their number as the value, aces are 11, everything else is 10).
*)
fun card_value (c: card) =
    (* Card -> Natural[2,11] *)
    (* produce value of card *)
    let val (s,r) = c
    in
	case r of
	    Ace => 11
	  | King => 10
	  | Queen  => 10
	  | Jack => 10
	  | Num V  => V
    end
	

(* Part C
Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. 
It returns a list that has all the elements of cs except c. If c is in the list more than once, remove only the first one. 
If c is not in the list, raise the exception e. You can compare cards with =.
*)
fun remove_card (loc : card list, c : card, e : exn) =
    (* (listof Card) Card Exception -> Exception|(listof Card) *)
    (* returns list with c removed, otherwise produce exception *)
    let fun aux(loc,acc) =
	    case loc of
		[] => raise e
	      | x::loc' => if x = c
			   then (acc @ loc')
			   else aux(loc',x::acc)
    in
	aux(loc,[])
    end


(* Part D
Write a function all_same_color, which takes a list of cards and returns true if all the cards in the list are the same color.
*)
fun all_same_color (loc : card list) =
    (* (listof Card) -> Boolean *)
    (* produce true if all cards have same color *)
    case loc of
	[] => true
      | _::[] => true
      | first::(next::rest) => (card_color(first) = card_color(next) andalso all_same_color(next::rest))
       
       
(* Part E
Write a function sum_cards, which takes a list of cards and returns the sum of their values. 
Use a locally defined helper function that is tail recursive.
*)
fun sum_cards (loc : card list) =
    (* (listof Card) -> Natural *)
    (* produce sum of values of all cards *)
    let fun aux(loc,acc) =
	    case loc of
		[] => acc
	      | c::loc' => aux(loc',acc+card_value(c))
    in
	aux(loc,0)
    end

(* Part F
Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes the score as described above.
*)
fun score (loc : card list, goal : int) =
    (* (listof Card) Natural -> Natural *)
    (* produce score with current hand *)
    let val sum = sum_cards(loc)
	val reduce = all_same_color(loc)
    in
	if sum > goal
	then if reduce
	     then (3 * (sum - goal)) div 2
	     else 3 * (sum -goal)
	else if reduce
	     then (goal - sum) div 2
	     else (goal - sum)
    end
	

(* Part G
Write a function officiate, which “runs a game.” 
It takes a card list (the card-list) a move list (what the player “does” at each point), 
and an int (the goal) and returns the score at the end of the game after processing (some or all of) the moves in the move list in order. 
Use a locally defined recursive helper function that takes several arguments that together represent the current state of the game.
• The game starts with the held-cards being the empty list. As described above:
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards not having c and the card-list unchanged. 
    • If c is not in the held-cards, raise the IllegalMove exception.
• If the player draws and the card-list is (already) empty, the game is over. 
    • Else if drawing causes the sum of the held-cards to exceed the goal, the game is over (after drawing). 
    • Else play continues with a larger held-cards and a smaller card-list.
*)
fun officiate (loc : card list, lom : move list, goal : int) =
    (* (listof Card) (listof Move) Natural -> Natural *)
    (* "runs a game" and produce score at the end *)
    let fun game(hand,deck,lom) =
	    case lom of
		[] => score(hand,goal)
	      | m::lom' => case m of
			       Discard C => game(remove_card(hand,C,IllegalMove),deck,lom')
			     | Draw => case deck of
					   [] => score(hand,goal)
					 | card::deck' => if sum_cards(card::hand) > goal
							  then score(card::hand,goal)
							  else game(card::hand, deck',lom')
    in
	game([],loc,lom)
    end

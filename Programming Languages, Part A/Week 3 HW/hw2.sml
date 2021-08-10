(* Score: 97.5/100 *)

(* Provided Code *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Problem 1 *)

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


fun all_same_color (loc : card list) =
    (* (listof Card) -> Boolean *)
    (* produce true if all cards have same color *)
    case loc of
	[] => true
      | _::[] => true
      | first::(next::rest) => (card_color(first) = card_color(next) andalso all_same_color(next::rest))
       
       
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

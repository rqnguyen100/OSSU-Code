(* Score based on Auto-grader: 100/100 *)

(* Date is int*int*int
   interp. a date with a year, Month, and a day *)

(* Month is Natural[1,12] *)

fun is_older (d1 : int*int*int, d2 : int*int*int) =
    (* Date Date -> Boolean
       produce tree if d1 is a date that comes before d2 *)
    if #1 d1 = #1 d2
    then if #2 d1 = #2 d2
	       then #3 d1 < #3 d2
	       else #2 d1 < #2 d2
    else #1 d1 < #1 d2

fun number_in_month (lod : (int*int*int) list, m : int) =
    (* (listof Date) Month -> Natural
       produce number of dates in list that are in given month *)
    if null lod
    then 0
    else if #2 (hd lod) = m
         then 1 + number_in_month (tl lod,m)
         else number_in_month (tl lod,m)
			 
fun number_in_months (lod : (int*int*int) list, lom : int list) =
    (* (listof Date) (listof Month) -> Natural
       produce number of dates in list that are in any of the list of months*)
    if null lom
    then 0
    else number_in_month(lod, hd lom) + number_in_months(lod, tl lom)

fun dates_in_month (lod: (int*int*int) list, m : int) =
    (* (listof Date) Month -> (listof Date)
       produce (listof Date) composed of dates with given month *) 
    if null lod
    then []
    else if #2 (hd lod) = m
         then (hd lod)::dates_in_month(tl lod, m)
         else dates_in_month(tl lod, m)

fun dates_in_months (lod: (int*int*int) list, lom: int list) =
    (* (listof Date) (listof Month) -> (listof Date)
        produce (listof Date) composed of dates that are in (listof Month) *)
    if null lom
    then []
    else dates_in_month(lod, hd lom) @ dates_in_months(lod, tl lom)
						     
fun get_nth (los: string list, n: int) =
    (* (listof String) Natural -> String
       produce the nth element of the list
       - ASSUME: los has at least one element *)
    if n = 1
    then hd los
    else get_nth(tl los, n-1)
		
fun date_to_string (d: int*int*int) =
    (* Date -> String
       produce a string	of the form "Month Day, Year" with given date *)
    let val months = ["January","February","March","April","May","June","July","August","Sepetember","October","November","December"]
    in
	      get_nth(months,#2 d)^" "^Int.toString(#3 d)^", "^Int.toString(#1 d)
    end
	
    
fun number_before_reaching_sum (sum : int, loi : int list) =
    (* Natural (listof Natural) -> Natural
       produce a natural such that the first nth elements of the list add to less than the sum *)
    if (sum - (hd loi)) <= 0
    then 0
    else 1 + number_before_reaching_sum(sum-(hd loi), tl loi)
				       
fun what_month (day : int) =
    (* Natural[1,365] -> String
       produce what month that day is in as a String (ex. January) *)
    let val month_first_day = [32,60,91,121,152,182,213,244,274,305,335,366]
	fun find_month (day : int, month_days : int list) =
	    if day < (hd month_days)
	    then 1
	    else 1 + find_month (day, (tl month_days))
    in
	find_month(day, month_first_day)
    end
	
fun month_range (day1 : int, day2 : int) =
    (* Natural[1,365] Natural [1,365] -> (listof Month)
       produce a (listof Month) where first element is month of day1 and 
       further elements are months of (day1 + 1 day) until day2 *)
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1,day2)
				      
fun oldest (lod : (int*int*int) list) =
    (* (listof Date) -> Date Option
       produce SOME d if the date d is the oldest date in the list *)
    if null lod
    then NONE
    else let
	fun oldest_nonempty (lod: (int*int*int) list) =
	    if null (tl lod)
	    then hd lod
	    else let val tl_ans = oldest_nonempty(tl lod)
		       in
		           if is_older(hd lod,tl_ans)
		           then hd lod
		           else tl_ans
		        end
      in
        	SOME (oldest_nonempty lod)
      end
	     

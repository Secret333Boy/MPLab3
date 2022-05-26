(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* a *)
fun all_except_option(str : string, str_list : string list) = 
   let
     fun hasString(str : string, []) = false | 
     hasString(str : string, (x : string)::xs) = same_string(str, x) orelse hasString(str, xs)
     fun filterByString(str : string, []) = [] |
     filterByString(str : string, (x : string)::xs) = if (same_string(str, x)) then filterByString(str, xs) else [x] @ filterByString(str, xs)
   in
     if hasString(str, str_list) then SOME(filterByString(str, str_list)) else NONE
   end
(* val a = all_except_option("a", ["b", "a", "c"]) *)
(* b *)
fun get_substitutions1([], s : string) = [] |
get_substitutions1((x : string list)::xs, s : string) = 
(case all_except_option(s, x) of 
NONE => [] | 
SOME (lst : string list) => lst) @ get_substitutions1(xs, s)
(* val b = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") *)
(* c *)
fun get_substitutions2(str_list_list : string list list, s : string) = 
   let
      fun get_result([], s : string) = [] |
      get_result((x : string list)::xs, s : string) = 
      (case all_except_option(s, x) of 
      NONE => [] | 
      SOME (lst : string list) => lst) @ get_result(xs, s)
   in
     get_result(str_list_list, s)
   end
(* val c = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") *)
(* d *)
fun similar_names(substitutions : string list list, {first:string,middle:string,last:string}) = 
   let
     fun get_similar_names([], {first:string,middle:string,last:string}) = [] |
     get_similar_names((x : string)::xs, {first:string,middle:string,last:string}) = 
     [{first=x, middle=middle, last=last}] @ get_similar_names(xs, {first=first, middle=middle, last=last})
   in
     [{first=first, middle=middle, last=last}] @ get_similar_names(get_substitutions2(substitutions, first), {first=first, middle=middle, last=last})
   end
(* val d = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) *)
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
(* a *)
fun card_color((suit, _) : card) : color = 
   case suit of
   Clubs => Black |
   Spades => Black |
   Diamonds => Red |
   Hearts => Red
(* val a = card_color((Clubs, Jack)) *)
(* b *)
fun card_value((_, rank) : card) : int = 
   case rank of
   Ace => 11 |
   King => 10 |
   Queen => 10 |
   Jack => 10 |
   Num(i) => i
(* val b = card_value((Clubs, Num(3))) *)
(* c *)
fun remove_card([], c : card, e : exn) = raise e |
remove_card((x::xs) : card list, c : card, e : exn) : card list = 
   if x = c then xs else [x] @ remove_card(xs, c, e)
(* val c = remove_card([(Clubs, Num(3)), (Clubs, Num(3)), (Clubs, Ace)], (Clubs, Num(3)), IllegalMove) handle IllegalMove => [(Clubs, Num(3)), (Clubs, Num(3)), (Clubs, Ace)] *)
(* d *)
fun all_same_color([]) : bool = true |
all_same_color(x::[]) : bool = true |
all_same_color((x1::x2::xs) : card list) : bool = if card_color(x1) = card_color(x2) then all_same_color([x2] @ xs) else false
(* val d = all_same_color([(Clubs, Num(3)), (Hearts, Ace), (Spades, Num(2))]) *)
(* e *)
fun sum_cards([]) : int = 0 |
sum_cards((x::xs) : card list) : int = card_value(x) + sum_cards(xs)
(* val e = sum_cards([(Clubs, Num(3)), (Hearts, Ace), (Spades, Num(2))]); *)
(* f *)
fun score(cl : card list, goal : int) : int = 
   let
     val sum = sum_cards(cl)
   in
     (if (sum > goal) then 3 * (sum - goal) else goal - sum) div (if all_same_color(cl) then 2 else 1)
   end
(* val f = score([(Clubs, Num(3)), (Spades, Ace), (Spades, Num(2)), (Spades, Num(6))], 16) *)
(* g *)
fun officiate(cl : card list, ml : move list, goal : int) = 
   let
      fun play(deck_list : card list, hand_list : card list, []) = score(hand_list, goal) |
      play(deck_list : card list, hand_list : card list, (move::remaining_moves) : move list) =
      let
         fun draw([]) = score(hand_list, goal) | draw((x::xs) : card list) = 
         if (sum_cards(x::hand_list) > goal) then score(x::hand_list, goal) else play(xs, x::hand_list, remaining_moves)
         fun discard(c : card) = play(deck_list, remove_card(hand_list, c, IllegalMove), remaining_moves)
      in
         case move of
         Draw => draw(deck_list) |
         Discard(card) => discard(card)
      end
   in
      play(cl, [], ml)
   end
(* val g = officiate([(Spades, Num(3)), (Clubs, Ace), (Hearts, Num(6))], [Draw, Discard(Spades, Num(3)), Draw, Draw], 12) *)
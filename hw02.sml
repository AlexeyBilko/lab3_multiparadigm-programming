(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
(* put your solutions for problem 1 here *)

(*a*)
fun all_except_option(str: string, list: string list) =
   case list of
      [] => NONE | x::list' => 
         if same_string(x, str) 
         then SOME(list')                  
         else case all_except_option(str, list') of
            NONE => NONE | SOME list' => SOME(x::list')

all_except_option("word2", ["word1", "word2", "word3", "word4"])

(*b*)
fun get_substitutions1(list: string list list, str: string) : string list =
   case list of
      [] => [] | x::xs =>
         case all_except_option(str, x) of
            NONE => get_substitutions1(xs, str) | SOME(names) => names @ get_substitutions1(xs, str);

get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")

(*c*)
fun get_substitutions2(list: string list list, str: string) : string list =
   let
      fun tail_recursive_func(lists, tmp) =
         case lists of
         [] => tmp | x::xs =>
            case all_except_option(str, x) of
               NONE => tail_recursive_func(xs, tmp) | SOME(names) => tail_recursive_func(xs, tmp @ names);
   in
      tail_recursive_func(list, [])
   end;

get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")

(*d*)
fun similar_names (list: string list list, { first = first_name, middle = middle_name, last = last_name }) =
   let
      fun get_names(list, names) = 
         case list of
         [] => names | x::xs => get_names(xs, names @ [{ first =  x, middle = middle_name, last = last_name }]);
   in
      get_names (
         get_substitutions1(list, first_name), [get_by_name(first_name)]
      )
end;

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(*a*)
fun card_color(card, _) =
   case card of
      Clubs => Black | Spades => Black | others => Red;

(*b*)
fun card_value(_, card) =
   case card of
      Ace => 11 | Num int => int | others => 10;

(*c*)
fun remove_card(cs: card list, c: card, e) =
   case cs of
      [] => raise e | x::xs =>
         if x = c
         then xs
         else x::remove_card(xs, c, e);

(*d*)
fun all_same_color(cs: card list) =
   case cs of
      [] => true | x::[] => true | x::y::xs =>
         card_color(x) = card_color(y) andalso all_same_color(y::xs);

(*e*)
fun sum_cards(cs: card list) =
   let
      fun tail_recursive_func(cs, sum) = 
         case cs of
         [] => sum | x::xs => sum + card_value(x) + sum_cards(xs);
   in
      tail_recursive_func(cs, 0)
   end;

(*f*)
fun score(cs: card list, goal: int) =
   let
      val total_score = sum_cards(cs);
      val previous_score = if total_score > goal then 3 * abs(total_score - goal) else abs(total_score - goal);
   in
      if all_same_color(cs)
      then previous_score div 2
      else previous_score
   end;

(*g*)
fun officiate(cards_list: card list, moves_list: move list, goal_score: int) =
   let
      fun next_move(cards_list, player_cards, moves_list) = 
         case moves_list of
         [] => score(player_cards, goal_score) | to_do::xs =>
            let
               fun get_card(cards_list, player_cards, moves_list) = 
                  case cards_list of
                     [] => score(player_cards, goal_score) | to_do::xs =>
                           if sum_cards(to_do::player_cards) > goal_score
                           then score(to_do::player_cards, goal_score)
                           else next_move(xs, to_do::player_cards, moves_list)

               fun discard_card(cards_list, player_cards, card, moves_list) =
                  next_move(cards_list, remove_card(player_cards, card, IllegalMove), moves_list);
            in
               case to_do of
                  Draw => get_card(cards_list, player_cards, xs) | Discard card => discard_card(cards_list, player_cards, card, xs)
            end;
   in
      next_move(cards_list, [], moves_list)
   end;
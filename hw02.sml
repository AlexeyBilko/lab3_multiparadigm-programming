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
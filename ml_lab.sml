(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Connor Grace
* connor.grace88@gmail.com
*
***************************************************************)

(* Command to quit program - OS.Process.exit(OS.Process.success); *)

(* Set datatype *)
datatype 'element set = Empty
   | Set of 'element * 'element set;

(* Warmup function *)
fun f [] = [] (* a if the list is empty, the empty list is returned*)
   | f (x::xs) = (x + 1) :: (f xs); (* b if the list is not empty, 1 is added to every value in the list*)

(* Function to check if a variable "e" is in set "set" *)
fun isMember e set =
   if
     set <> Empty   (* if set = Empty return false (e is not in set), if set is not empty procede with function *)
   then
     let
       val (Set s) = set;   (* create local variable from set that is a tuple (element, element set) instead of the set Set(element, element set) which is inputed as a parameter *)
     in
       if
         #1(s) <> e   (* if first element in set is not e *)
       then
         isMember e (#2(s))   (* then call isMember with the "tail" of the set *)
       else
         true   (* else e is a member of set *)
     end
   else
     false;   (* e is not in set *)

(* Function to convert a list to a set (The set parameter must be the Empty set) *)
fun list2Set list set =
   if
     length(list) > 0   (* if list is not empty, procede with function, if empty, return set *)
   then
     let
       val s = Set(List.last(list), set)    (* create a local variable that is a Set of the last element in the list followed by the set input in the fuction parameter *)
       val l = List.take(list, length(list)-1)    (* create local list variable that is the list input without the last element *)
     in
       if
         isMember (List.last(list)) set   (* if the last element of the list is already in the set, do not add it again *)
       then
         list2Set l set   (* recursively call list2set with the local variable l and the same set parameter *)
       else
         list2Set l s   (* recursively call list2set with the local variable l and the new set containing the last element of list *)
     end
   else
     set;   (* return the set *)

(* Function to return the union of two sets set1 and set2 *)
fun union set1 set2 =
   if
     set1 <> Empty    (* if set = Empty return set2, if set is not empty procede with function *)
   then
     let
       val (Set s) = set1   (* create local variable s from set that is a tuple (element, element set) instead of the set Set(element, element set) which is inputed as a parameter *)
       val i = #1(s)    (* create local variable i that is the first element of set1 *)
       val j = #2(s)    (* create local variable j that is the second elemnt of set1 (this is a set) *)
       val f = Set(i, set2)   (* create a local variable j that is set2 with the first element of set1 (i) added to it *)
     in
       if
         isMember i set2    (* if i is in set2, then don't add to union set, else add it *)
       then
         union j set2   (* recursively call union with j and set2 *)
       else
         union j f    (* recursively call union with j and f *)
     end
   else
     set2;    (* return the union set *)

(* Function to return the intersection of two sets set1 and set2 (The set3 parameter must be the Empty set, set3 will be returned as the intersect set) *)
fun intersect set1 set2 set3 =
   if
     set1 <> Empty    (* if set = Empty return set3, if set is not empty procede with function *)
   then
     let
       val (Set s) = set1   (* create local variable s from set that is a tuple (element, element set) instead of the set Set(element, element set) which is inputed as a parameter *)
       val i = #1(s)    (* create local variable i that is the first element of set1 *)
       val j = #2(s)    (* create local variable j that is the second elemnt of set1 (this is a set) *)
       val f = Set(i, set3)   (* create a local variable j that is set2 with the first element of set1 (i) added to it *)
     in
       if
         isMember i set2    (* if i is in set2, add to intersect set, else don't add *)
       then
         intersect j set2 (union (Set(i, Empty)) set3)    (* recursively call intersect with j, set2, and i added to set3 *)
       else
         intersect j set2 set3    (* recursively call intersect with j, set2, and set3 *)
     end
   else
     set3;    (* return the intersection set *)

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set ([1, 3, 2]) (Empty);
list2Set ([#"a", #"b", #"c"]) (Empty);
list2Set ([]) (Empty);
list2Set ([6, 2, 2]) (Empty);
list2Set (["x", "y", "z", "x"]) (Empty);

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember ("one") (list2Set (["1", "2", "3", "4"]) (Empty));
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set (["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"]) (Empty);
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set (["green", "eggs", "and"]) (Empty)) (list2Set (["ham"]) (Empty)));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set (["stewed", "tomatoes", "and", "macaroni"]) (Empty)) (list2Set (["macaroni", "and", "cheese"]) (Empty)) (Empty));

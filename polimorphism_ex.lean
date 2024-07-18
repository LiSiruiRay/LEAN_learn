def List.find_last_entry (α : Type) (l : List α) : Option α :=
  match l with
    | [] => none
    | a :: b =>
      match b with
        | [] => some a
        | c :: d => List.find_last_entry α b

def List.find_last_entry_simp (α : Type) (l : List α) : Option α :=
  match l with
    | [] => none
    | a :: [] => some a
    | _ :: b => List.find_last_entry_simp α b

def test_list : List String := ["a", "b", "c", "d"]
#eval List.find_last_entry_simp String test_list

def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
  match xs with
    | [] => none
    | a :: b => if predicate a then a else List.findFirst? b predicate

def is_b (a : String) : Bool := a == "c"
#eval List.findFirst? test_list is_b

-- Write a function Prod.swap that swaps the two fields in a pair. Start the definition with def Prod.swap {α β : Type} (pair : α × β) : β × α :=

#check Prod
def Prod.swap {α β : Type} (pair : α × β) : β × α := (pair.snd, pair.fst)

def test_prod : Prod String (Prod Nat Int) := ("123", 45, -5)

#eval Prod.swap test_prod

-- Rewrite the PetName example to use a custom datatype and compare it to the version that uses Sum.

--- TODO: ask

-- inductive PetName (α : Type) : Type where
--   | Dog : α → String
--   | Cat : α → String

/-

Write a function zip that combines two lists into a list of pairs. The resulting list should be as long as the shortest input list. Start the definition with `def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=`.

-/

def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
  match xs with
    | [] => List.nil
    | a :: b =>
      match ys with
        | [] => List.nil
        | c :: d => List.cons (Prod.mk a c) (zip b d)

def list_one : List String := ["a", "b", "c", "d"]
def list_two : List Int := [-1, 2, 3]

#eval zip list_one list_two
#eval zip list_two list_one


/-
Write a polymorphic function `take` that returns the first $n$
 entries in a list, where n
 is a Nat. If the list contains fewer than n entries, then the resulting list should be the input list. #eval take 3 ["bolete", "oyster"] should yield ["bolete", "oyster"], and #eval take 1 ["bolete", "oyster"] should yield ["bolete"].
-/

def take {α : Type} (n : Nat) (l : List α) : List α :=
  match n with
    | 0 => List.nil
    | Nat.succ k =>
      match l with
        | [] => List.nil
        | a :: b => List.cons (a) (take k b)

#eval take 3 ["bolete", "oyster"]
#eval take 1 ["bolete", "oyster"]
-- #eval List.cons ("List.nil") (List.cons ("List.nil") (List.nil))
-- TODO: so nil → nil has no effect?

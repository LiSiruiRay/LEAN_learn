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

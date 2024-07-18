def length {α : Type} (l : List α) : Nat :=
  match l with
    | [] => 0
    | _ :: b => 1 + length b

def length_simp (l : List α) : Nat :=
  match l with
    | [] => 0
    | _ :: b => 1 + length b

def length_simp_simp : List α → Nat
  | [] => 0
  | _ :: b => 1 + length b


def drop : Nat → List α → List α
  | _, [] => []
  | 0, x => x
  | Nat.succ k, _ :: b => drop k b

def test_list : List Nat := [1, 2, 3, 4, 5]

#eval drop 4 test_list

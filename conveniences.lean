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


def fromOption (defualt : α) : Option α → α
  | none => defualt
  | some x => x

def unzip : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xyz => (x :: (unzip xyz).fst, y :: (unzip xyz).snd)



-- def unzip_faster : List (α × β) → List α × List β
--   | [] => ([], [])
--   | (x, y) :: xyz
--   let unzipped : List α × List β := unzip_faster xyz
--   => x :: unzipped.fst × y :: unzipped.snd

def unzip_faster : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xyz =>
    let unzipped : List α × List β := unzip_faster xyz
    (x :: unzipped.fst, y :: unzipped.snd)


def reverse (xs : List α) : List α :=
  let rec helper : List α → List α → List α
      | [], sofar => sofar
      | a :: as, sofar => helper as (a :: sofar)
    helper xs []

def test_list : List Int := [1, 2, 3, 4, 5]
#eval reverse test_list

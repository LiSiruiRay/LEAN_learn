-- This module serves as the root of the `LeanLearn` library.
-- Import modules here that should be built as part of the library.
import «LeanLearn».Basic


def add1 (n: Nat) : Nat := n + 1

#eval add1 15
#eval 1 + 7

#eval String.append "Hello, " "World"

#eval String.append "It is, " (if 1 < 2 then "yet" else "no")

-- exercise

#eval 42 + 19
#eval String.append "A" (String.append "B" "C")
#eval String.append (String.append "A" "B") "C"
#eval if 3 == 3 then 5 else 7
#eval if 3 == 3 then "equal" else "not equal"

--- Types

#eval (1 - 2: Int)
#eval 1 -2

#check 1 - 2

--- functions and definitions
def helloo := "Hello"
#check helloo

def complicated_hello : String := "hello"

def maximum (a : Nat) (b : Nat) : Nat :=
  if a < b then b else a

#eval maximum 5 10
#check maximum
#check (maximum)
#eval String.append "1" "2"

def joinStringsWith (a : String) (b : String) (c : String) : String :=
  String.append (String.append b a) c

#eval joinStringsWith ", " "one" "and another"

#check joinStringsWith

--- Structure
structure Point where
  x : Float
  y : Float

deriving Repr
#check Point

def origin: Point := {x := 0, y := 0}

#eval origin

def add_two_points (p1: Point) (p2: Point): Point :=
  {x := p1.x + p2.x, y := p1.y + p2.y}

-- def zeroX (p: Point) : Point :=
--   {x := 0, y := p.y}

def zeroX (p : Point) : Point :=
  { x := 0, y := p.y }

def test_point : Point := {x := 5, y := 6}

#eval test_point

#eval zeroX test_point
#eval test_point

def test_point : Point := zeroX test_point

structure RectangularPrism where
  height : Float
  width : Float
  depth : Float
deriving Repr

def volume (r : RectangularPrism) : Float :=
  Float.mul (Float.mul r.height r.width)  $ r.depth

-- structure Segment

def plus (n: Nat) (k: Nat) :=
match k with
  | Nat.zero => n
  | Nat.succ k' => Nat.succ (plus n k')

--- polymorphism
structure PPoint (α : Type) where
  x : α
  y : α

deriving Repr

def test_ppoint : PPoint Nat := {x := 5, y := 6}
#eval test_ppoint


inductive Sign where
  | pos
  | neg

def posOrNegThree (s : Sign) : match s with | Sign.pos => Nat | Sign.neg => Int :=
  match s with
    | Sign.pos => 3
    | Sign.neg => -3

#eval posOrNegThree Sign.neg
#check posOrNegThree Sign.neg

inductive List_test (α : Type) where
  | nil : List_test α
  | cos : α → List_test α → List_test α

def string_lsit : List String := ["a", "b"]

-- def length (α : Type) (l : List α) : Nat :=
--   match l with
--     | List.nil => Nat.zero
--     | List.cos y ys => length Nat l with cos := Nat → List


def length (α : Type) (l : List α) : Nat :=
  match l with
    | List.nil => Nat.zero
    | List.cons _ ys => Nat.succ (length α ys)

-- option as null
def List.head_self? (α : Type) (l : List α) : Option α :=
  match l with
    | List.nil => none
    | List.cons y ys => some y

#eval [].head_self? (α := Nat)

-- prod
def fives : String × Nat := ("f", 5)
def fives_longer : String × Nat := {fst := "f", snd := 5}

-- sum
def petName : Type := Sum String String
-- ⊕

def list_petnames : List petName :=
 [Sum.inl "Spot", Sum.inr "Tiger", Sum.inl "Fifi", Sum.inl "Rex", Sum.inr "Floof"]

def count_dog_names (l : List petName) : Nat :=
  match l with
    | List.nil => Nat.zero
    | List.cons a l =>
      match a with
        | Sum.inl d => Nat.succ (count_dog_names l)
        | Sum.inr c => count_dog_names l

#eval count_dog_names list_petnames

def cdn (l : List petName) : Nat :=
  match l with
    | [] => Nat.zero
    | Sum.inl a :: b => 1 + cdn b
    | Sum.inr a :: c => cdn c

-- exercise
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

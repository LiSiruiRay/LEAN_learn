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

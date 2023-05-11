From Coq Require Import QArith.
From Coq Require Import Extraction.
Require Coq.extraction.ExtrHaskellNatInt.


Extraction Language Haskell.

(* Extract Coq's Q to Haskell's Rational *)
Extract Inductive Q => "Rational" [ "toRational" ].

(* Extract Coq's Qadd to Haskell's (+) *)
Extract Constant Qplus => "(+)".

(* Extract Coq's Qmult to Haskell's ( * ) *)
Extract Constant Qmult => "(*)" .

(* Extract Coq's Qopp to Haskell's negate *)
Extract Constant Qopp => "negate".

(* Extract Coq's Qsub to Haskell's (-) *)
Extract Constant Qminus => "(-)".

(* Extract Coq's Qdiv to Haskell's (/) *)
Extract Constant Qdiv => "(/)".

(* Extract Coq's Qeq to Haskell's (==) *)
Extract Constant Qeq => "(==)".

(* Extract Coq's Qle to Haskell's (<=) *)
Extract Constant Qle => "(<=)".

(* Extract Coq's Qlt to Haskell's (<) *)
Extract Constant Qlt => "(<)".

(* Extract Coq's Qcompare to Haskell's compare *)
Extract Constant Qcompare => "compare".
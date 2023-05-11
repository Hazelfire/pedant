Require Import Coq.Arith.Arith.
Require Import Coq.Strings.String.
Require Import Coq.FSets.FMapInterface.
Require Import Coq.FSets.FMapFacts.
Require Import Coq.FSets.FMapAVL.
Require Import Coq.Bool.Bool.
Require Import Coq.Structures.OrderedTypeEx.
Require Import ZArith.
Require Import QArith.
Require Import Pedant.MyExtraction.
Require Import Extraction.
Require ExtrHaskellBasic.
Require ExtrHaskellZInt.
Open Scope Q_scope.

Extraction Language Haskell.

Definition BaseUnit := nat.
Module UnitPairOrderedType : OrderedType := PairOrderedType Nat_as_OT Z_as_OT.

Module UnitSet := FMapAVL.Make Nat_as_OT.


Definition Unit := UnitSet.t Z.

Extract Constant Unit => "Data.Map.Strict.Map".

Inductive Expr : Type :=
  | Num : Q -> Unit -> Expr
  | Add : Expr -> Expr -> Expr
  | Sub : Expr -> Expr -> Expr
  | Mul : Expr -> Expr -> Expr
  | Div : Expr -> Expr -> Expr.

Print remove.

Definition Z_eq_dec : forall x y : Z, {x = y} + {x <> y} := Z.eq_dec.

Lemma Zpair_eq_dec : forall x y : Z * Z, {x = y} + {x <> y}.
Proof.
  decide equality; apply Z_eq_dec.
Defined.

Definition zip_units (f: Z -> Z -> Z) (m1 m2 : Unit) : Unit :=
  UnitSet.mapi
    (fun k v1 =>
      match UnitSet.find k m2 with
      | Some v2 => (f v1 v2) : Z
      | None => v1 : Z
      end)
    m1.

Definition unit_mult (u1 u2 : Unit) : Unit :=
  zip_units Z.add u1 u2.

Definition unit_div (u1 u2 : Unit) : Unit :=
  zip_units Z.sub u1 u2.

Print UnitSet.
Definition unit_eqb := UnitSet.equal Z.eqb.

Fixpoint eval (e : Expr) : option (Q * Unit) :=
  match e with
  | Num n u => Some (n, u)
  | Add e1 e2 =>
    match eval e1, eval e2 with
    | Some (n1, u1), Some (n2, u2) =>
      if unit_eqb u1 u2 then Some (n1 + n2, u1) else None
    | _, _ => None
    end
  | Sub e1 e2 =>
    match eval e1, eval e2 with
    | Some (n1, u1), Some (n2, u2) =>
      if unit_eqb u1 u2 then Some (n1 - n2, u1) else None
    | _, _ => None
    end
  | Mul e1 e2 =>
    match eval e1, eval e2 with
    | Some (n1, u1), Some (n2, u2) => Some (n1 * n2, unit_mult u1 u2)
    | _, _ => None
    end
  | Div e1 e2 =>
    match eval e1, eval e2 with
    | Some (n1, u1), Some (n2, u2) =>
      if Qeq_bool n2 0 then None else Some (n1 / n2, unit_div u1 u2)
    | _, _ => None
    end
  end.

Fixpoint rawEval (e : Expr) : option Q :=
  match e with
  | Num n u => Some n
  | Add e1 e2 =>
    match rawEval e1, rawEval e2 with
    | Some n1, Some n2 => Some (n1 + n2)
    | _, _ => None
    end
  | Sub e1 e2 =>
    match rawEval e1, rawEval e2 with
    | Some n1, Some n2 => Some (n1 - n2)
    | _, _ => None
    end
  | Mul e1 e2 =>
    match rawEval e1, rawEval e2 with
    | Some n1, Some n2 => Some (n1 * n2)
    | _, _ => None
    end
  | Div e1 e2 =>
    match rawEval e1, rawEval e2 with
    | Some n1, Some n2 =>
      if Qeq_bool n2 0 then None else Some (n1 / n2)
    | _, _ => None
    end
  end.

Definition DimensionMultiplier : Type := BaseUnit -> Q.

Definition RawConversionFunction : Type :=
  option Q -> DimensionMultiplier -> option Q.

Fixpoint ppower (q : Q) (n : positive) : Q :=
  match n with
  | xH => q
  | xO p => let r := ppower q p in Qmult r r
  | xI p => let r := ppower q p in Qmult q (Qmult r r)
  end.

Definition power (q : Q) (n : Z) : Q :=
  match n with
  | Z0 => 1
  | Zpos p => ppower q p
  | Zneg p => Qinv (ppower q p)
  end.

Definition multiplyUnit (q : Q) (u : Unit) (dm : DimensionMultiplier) : Q :=
  UnitSet.fold (fun unit_key exponent acc => Qmult acc (power (dm unit_key) exponent)) u 1 * q.

Extract Constant UnitSet.fold =>"Data.Map.Strict.fold".

Fixpoint multiplyExpression (e : Expr) (dm : DimensionMultiplier) : Expr :=
  match e with
  | Num q u => Num (multiplyUnit q u dm) u
  | Add e1 e2 => Add (multiplyExpression e1 dm) (multiplyExpression e2 dm)
  | Sub e1 e2 => Sub (multiplyExpression e1 dm) (multiplyExpression e2 dm)
  | Mul e1 e2 => Mul (multiplyExpression e1 dm) (multiplyExpression e2 dm)
  | Div e1 e2 => Div (multiplyExpression e1 dm) (multiplyExpression e2 dm)
  end.

Definition someQEq (q1 q2 : option Q): Prop :=
  match q1, q2 with
  | Some a, Some b => Qeq a b
  | None, None => True
  | _, _ => False
  end.

Definition RawConversionFunctionValid (g: RawConversionFunction) (e: Expr) := forall (dm: DimensionMultiplier), someQEq (rawEval (multiplyExpression e dm)) (g (rawEval e) dm).

Record ConversionFunction (e: Expr) := {
  g : RawConversionFunction;
  valid : RawConversionFunctionValid g e
}.

Definition numConversionFunction (q : Q) (u : Unit) : ConversionFunction (Num q u):=
  let g := fun resultOption dim =>
    match resultOption with
    | Some result => Some (multiplyUnit q u dim)
    | None => None
    end
  in {|
    g:= g;
    valid:= (fun (d: DimensionMultiplier) => eq_refl) : RawConversionFunctionValid g (Num q u)
    |}.

Definition addConversionFunctionRaw (q1 q2 : Q) (u : Unit) : RawConversionFunction :=
  let g := fun resultOption dim =>
    match resultOption with
    | Some result => Some (multiplyUnit (q1 + q2) u dim)
    | None => None
    end
  in g.

Module UnitSetFacts := WProperties_fun(Nat_as_OT)(UnitSet).

Lemma addConversionFunctionRawValid (q1 q2: Q) (u: Unit): RawConversionFunctionValid (addConversionFunctionRaw q1 q2 u) (Add (Num q1 u) (Num q2 u)).
Proof.
unfold RawConversionFunctionValid.
simpl.
unfold multiplyUnit.
intros.
field.
Qed.
Extraction Language Haskell.

Extraction "Numerics.hs" multiplyExpression.


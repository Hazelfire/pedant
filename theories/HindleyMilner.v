Require Import Coq.Lists.List.
Import ListNotations.
Require Import Coq.Logic.Decidable.
Require Import Coq.Strings.String.
Require Import Coq.Arith.PeanoNat.

Section HindleyMilner.

Variable atom : Type.
Variable atom_eq_dec : forall x y : atom, {x = y} + {x <> y}.
Variable fresh_atom : list atom -> atom.
Hypothesis fresh_atom_spec : forall xs, ~ In (fresh_atom xs) xs.

Inductive expr : Type :=
| EVar : atom -> expr
| EAbs : atom -> expr -> expr
| EApp : expr -> expr -> expr
| ELet : atom -> expr -> expr -> expr.

Inductive type : Type :=
| TVar : atom -> type
| TArrow : type -> type -> type.

Inductive context : Type :=
| Empty : context
| Extend : atom -> type -> context -> context.

Inductive typing : context -> expr -> type -> Prop :=
| TyVar : forall Gamma x t, 
    (Extend x t Empty) = Gamma ->
    typing Gamma (EVar x) t
| TyAbs : forall Gamma x e t1 t2,
    typing (Extend x t1 Gamma) e t2 ->
    typing Gamma (EAbs x e) (TArrow t1 t2)
| TyApp : forall Gamma e1 e2 t1 t2,
    typing Gamma e1 (TArrow t1 t2) ->
    typing Gamma e2 t1 ->
    typing Gamma (EApp e1 e2) t2
| TyLet : forall Gamma x e1 e2 t1 t2,
    typing Gamma e1 t1 ->
    typing (Extend x t1 Gamma) e2 t2 ->
    typing Gamma (ELet x e1 e2) t2.

Fixpoint substitute (x : atom) (u : type) (t : type) : type :=
  match t with
  | TVar y => if atom_eq_dec x y then u else t
  | TArrow t1 t2 => TArrow (substitute x u t1) (substitute x u t2)
  end.

Inductive unifies : type -> type -> Prop :=
| UVar : forall x t, unifies (TVar x) t
| UArrow : forall t1 t2 t3 t4,
    unifies t1 t3 ->
    unifies t2 t4 ->
    unifies (TArrow t1 t2) (TArrow t3 t4).

Fixpoint type_eq_dec (t1 t2 : type) : bool :=
    match t1, t2 with
    | TVar a1, TVar a2 => if atom_eq_dec a1 a2 then true else false
    | TArrow t1a t1b, TArrow t2a t2b =>
        andb (type_eq_dec t1a t2a) (type_eq_dec t1b t2b)
    | _, _ => false
    end.
    
Fixpoint atoms_type (t : type) : list atom :=
    match t with
    | TVar x => [x]
    | TArrow t1 t2 => atoms_type t1 ++ atoms_type t2
    end.

Fixpoint atoms_type_context (Gamma : context) : list atom :=
    match Gamma with
    | Empty => []
    | Extend x t G => x :: atoms_type t ++ atoms_type_context G
    end.

Fixpoint lookup_context (Gamma: context) (x: atom) : option type :=
    match Gamma with
    | Empty => None
    | Extend y t G => if atom_eq_dec x y then Some t else lookup_context G x
    end.

Fixpoint infer (e : expr) (Gamma : context) : option type :=
  match e with
  | EVar x => lookup_context Gamma x
  | EAbs x e =>
    let t1 := fresh_atom (atoms_type_context Gamma) in
    let t2 := TVar (fresh_atom (t1 :: atoms_type_context Gamma)) in
    match infer e (Extend x (TVar t1) Gamma) with
    | Some t2 => Some (TArrow (TVar t1) t2)
    | None => None
    end
  | EApp e1 e2 =>
    match infer e1 Gamma, infer e2 Gamma with
    | Some (TArrow t1 t2), Some t1' =>
      if type_eq_dec t1 t1'
      then Some t2
      else None
    | _, _ => None
    end
  | ELet x e1 e2 =>
    match infer e1 Gamma with
    | Some t1 =>
      infer e2 (Extend x t1 Gamma)
    | None => None
    end
  end.

End HindleyMilner.
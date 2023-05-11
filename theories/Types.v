Require Import Coq.Strings.String.
Require Import ZArith.
Require Import Coq.Sets.Ensembles.
Require Import
  Coq.FSets.FMapList
  Coq.Structures.OrderedTypeEx.
Require Import
  Coq.Structures.OrderedType.


Inductive primDim : Set :=
  | litDim : string -> primDim
  | polyDim : string -> primDim.

Module primDim_as_OT <: UsualOrderedType.
  Definition t := primDim.
  Definition eq := @eq primDim.
  Definition eq_sym := @eq_sym t.
  Definition eq_trans := @eq_trans t.
  Definition eq_refl := @eq_refl t.

  Definition lt (a : primDim) (b: primDim) :=
    match a, b with
    | litDim x, litDim y => String_as_OT.lt x y
    | litDim x, polyDim y => True
    | polyDim x, litDim y => False
    | polyDim x, polyDim y => String_as_OT.lt x y
    end.

  Lemma lt_trans : forall x y z : t, lt x y -> lt y z -> lt x z.
  Proof.
    intros x y z H1 H2.
    destruct x; destruct y; destruct z; trivial; apply (String_as_OT.lt_trans s s0 s1); auto ;contradiction.
  Qed.

  Lemma lt_not_eq : forall x y : t, lt x y -> ~ eq x y.
  Proof.
    intros x y islt eq.
    destruct x.
    - destruct y.
      + simpl in islt.
        unfold primDim_as_OT.eq in eq.
        inversion  eq.
        apply (String_as_OT.lt_not_eq s s0 islt) ; assumption.
      + unfold primDim_as_OT.eq in eq.
        discriminate.
    - destruct y.
      + contradiction.
      + unfold primDim_as_OT.eq in eq.
        inversion eq.
        apply (String_as_OT.lt_not_eq s s0 islt); assumption.
  Qed.

  Definition compare (a b : primDim) : Compare lt eq a b :=
     match a, b with
    | litDim x, litDim y => String_as_OT.compare x y
    | litDim x, polyDim y => Lt
    | polyDim x, litDim y => Gt
    | polyDim x, polyDim y => String_as_OT.compare x y
    end.
  Definition eq_dec := eq_nat_dec.



End primDim_as_OT.

Module Import dimMap := FMapList.Make(primDim_as_OT).

Inductive dimension : Set :=
  | normDim : list (primDim * Z) -> dimension
  | powDim : list (primDim * Z) -> dimension.


Inductive type : Set :=
  | baseType : dimension -> type
  | listType : type -> type
  | dictType : (string -> type) -> type
  | polyDictType : (string -> type) -> type
  | funcType : type -> type -> type.


Record substitution : Set := mkSubstitution
                               { subDimension : list (string * dimension)
                               ; subTypes : list (string * type)
                               }.

Class Types A : Type :=
  { ftv : A ->  Ensemble string;
    app : substitution -> A -> A
  }.

Search ((?a -> bool) -> list ?a -> option ?a).

Open Scope string_scope.

Search ((?a -> ?b) -> option ?a -> option ?b).
Check eqb.

Definition lookup {A : Type} (name : string) (substitution : list (string * A)) : option A :=
  option_map snd (List.find (fun x => fst x =? name) substitution).

Print subDimension.

Definition lookupFreeVariables (dimMap : list (primDim * Z)) : Ensemble Z :=


Instance typesDimension : Types dimension :=
  { ftv :=
    fun a:dimension =>
         match a with
         | litDim _ => Empty_set _
         | polyDim v => Singleton _ v
         end
  , app :=
      fun (sub:substitution) (a:primDim) =>
        match a with
        | litDim b => litDim b
        | polyDim v =>
            match lookup v (subDimension substitution) with
              Some sub

         
  }.

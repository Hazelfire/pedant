Load Numerics.
Import ListNotations.

Inductive PrimitiveType : Type :=
  | Number
  | Boolean.

Module RecordMap := FMapAVL.Make Nat_as_OT.

Inductive PedantType : Type :=
  | Primative : PrimitiveType -> PedantType
  | UnorderedSet : PedantType -> PedantType
  | Record : list (nat * PedantType) -> PedantType.

Definition impact : nat := 0.
Definition given_placement : nat := 1.
Definition counterfactualAdjustment := 2.

Definition populationIcapsType := UnorderedSet (Record [(impact, Primative Number)]).

(* Just thinking about how I want this to come out for ICAPs
I'm thinking, you define impact as:
groupType : Treatment | Control | Other
importance : set({id: ConceptionID, from: ID, to: ID, relative_importance : importance[to] importance[from]^{-2}})
population : set({id: ID, impact: I, occured: boolean, group: groupType})
impact: sum(impact|occured) > sum(impact|!occured)
impact = sum(impact | occured) / length_P(impact | occured)

counterfactual_scenarios = set({got_placement: boolean, personId: PersonId})
population_icap = set({personId: PersonId, impact: I, given_placement: boolean, counterfactualAdjustment: E_counterfactual_scenarios(got_placement | personId)})
ICAPS = sum((0 - counterfactual_adjustment) * impact * given_placement)
ICAPS = sum((0 - E(got_placement | personId & !given_placement)) * impact * got_placement)
ICAPS = sum(impact * given_placement -  E(got_placement | personId & !given_placement)) * impact * got_placement)
// Rule, you can only combine sets that are of the same condition. considering impact is | given_placement this fails.  ICPS = impact * got_placement  - impact * got_placement | !occured
*)
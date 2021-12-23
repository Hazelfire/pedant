# Operations

This is a table of what is valid and not valid in pedant, and how operations
change units.

A full table of operations in spreadsheet form [is available here](https://docs.google.com/spreadsheets/d/1KplvmW2t0QW4mk1WbNkQLf2_ZxG0Lvd6aq14nHqMJtI/edit?usp=sharing).

## Dimensionless quantities

Dimensionless quantities can be `+`, `-`, `*`, `/`, `^` to other dimensionless
quantities to make dimensionless quantities. `ln x` where x is a dimensionless
quantity is also dimensionless.

This has the curious effect of allowing you to, if you wish, write calculations
without units. All operations with numbers without units are valid.

## Normal units

A normal unit is a number such as `5 years` or `2 meters`. A normal unit can
be `+` and `-` with other items of the same unit. When `*` or `/`, it will multiply
and divide the units. 

Normal units cannot be used in the base of an `^`. Normal
units can be used in the exponent of `^`. If the base is a dimensionless quantity,
it becomes the power unit version of a normal unit, say ` 1 ^ (5 years)` becomes
units `^years`. If the base is a power, then the power unit multiplies together,
For instance `(1 ^meters)^(5 years)` is in the units `^meters years`. If the
units cancel out in the exponent, say `(1 ^years-1)^(5 years)` then the resulting
unit is dimensionless.

Taking the `ln x` of a normal unit is technically still dimensionful according
to the [theory](theory.md), however I have yet to find practical use for the construct.

# Power Units
Power units cannot be `+` or `-` to any unit, including itself. `*` and `/` two
items of the same power unit combine them into the same power unit. Say `5 ^years-1 * 10 ^years-1`
is of units `^years-1` and so is `5 ^years-1 / 10 ^years-1`. Power units can be
used in the base but not the exponent of a `^`, and the behaviour is detailed
in the Normal Units section.

Taking the logarithm of a power unit gives the normal unit version. For instance
`ln (5 ^years-1)` is in units `years-1`.

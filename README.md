# Pedant
Pedant is a minimal math DSL. It's originally designed for use in cost
effectiveness analysis. However, it can be used for any important calculations.

The goal of pedant is to make sure that it's difficult or impossible to make 
math and stats errors, and also allow for the full enumeration of the assumptions
used for a calculation. Currently, its only feature is dimensional analysis, but
I'm planning to add stats and constraints on stats in the future.

# Basic syntax
The syntax of pedant is really simple to learn. It's simply a collection of
math formulas. It would be easier to understand by simply looking at the examples
in `examples`.

Each line can either be empty or not have a declaration of a variable on it. The variable is equated to a term:

```pedant
donation_size = 100000 usd
```

On the right is the value for that term.

That value can make up an expression:

```pedant
consumption_possible_by_funds = (1 - percent_of_transfers_invested) * size_of_transfer_per_person / duration_of_initial_consumption
```

The available operators are:
 - `*` multiplication
 - `/` division
 - `^` exponentiation
 - `+` addition
 - `-` subtraction

There is also one function currently available, the natural logarithm: `ln`. 
Functions are applied by whitespace, like functional languages. For instance `ln 5`.

Paranthesis can be used to impact the order of operations

Unlike some functional languages, only variables that have been declared higher in the file can be referenced.

C style line and block comments are also available:

```pedant
// This is a line comment
/* 
  This is a block comment
*/
```

# Dimensional Analysis in Pedant
A dimensional checker runs over pedant to ensure that there are no dimensional
violations.

If you don't want a dimensional checker, all dimensionless operations are valid.
Meaning you simply need to not add units to your code.

However, the basics of dimensional checking is that you can assign a type to value,
such as

```pedant
average_household_size = 4.7 person
```

This indicates that average_household_size is in units `person`.

The checker is really simple, it basically follows two main rules:

- When multiplying two values together, we must also multiply the units. Also you divide the units when you divide two values
- You cannot add or subtract two values of different units

So for instance, the following construction is valid:
```pedant
bag_count = bag
apples_per_bag = apple bag-1
total_apples = apples_per_bag * bag_count
```

as `total_apples` would be of units `apple`.

But this is invalid:

```pedant
bag_count = bag
apples_per_bag = apple bag-1
total_apples = apples_per_bag + bag_count
```

As you are adding two values of different units.

This helps you ensure that the math and reasoning behind your calculations are correct.

As above in `bag-1`, a dimension can have a number after it that indicates what the power of that dimension is. In this case, this means `1/bag`, or "per bag". You can also do things like `m2` to mean meters squared.

## Extension of Dimensional Analysis
For my purposes, this actually wasn't a good enough definition of
dimensional analysis. For instance, in standard dimensional analysis, you cannot take the log or exponent of a dimensionful quantity. However, in the field of economics, you often have a time variable in an exponent when calculating things like interest or discount rates:

```pedant
princple_plus_interest = principle * (rate ^ duration)
```

As of such, I designed an extension to dimensional analysis that allows
you to take powers of dimensionful quantities.

A power dimension is a dimension that starts with a `^` character, such as:

```pedant
discount_rate = 1.04 ^year-1
```

There are only two valid places you can put a variable with a power dimension, in a logarithm, or the base of an exponential. For instance:

```pedant
value = start_value * (discount_rate ^ (-(3 year))
```

In this case, the dimension of the discount_rate and the exponent multiply together, being `year-1` and `year`, so they cancel each other out.


In a logarithm:
```pedant
value = ln(discount_rate)
```
The power dimension simply transforms back into a normal dimension, into `year-1`.

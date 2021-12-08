# Pedant
Pedant is a minimal math DSL. It's originally designed for use in cost
effectiveness analysis. However, it can be used for any important calculations.

The goal of pedant is to make sure that it's difficult or impossible to make 
math and stats errors, and also allow for the full enumeration of the assumptions
used for a calculation. Currently, its only feature is dimensional analysis, but
I'm planning to add stats and constraints on stats in the future.

It might be better to think of pedant as a replacement for excel rather than a
programming language.

# Basic syntax
The syntax of pedant is really simple to learn. It's simply a collection of
math formulas. It would be easier to understand by simply looking at the examples
in `examples`.

Each line can either be empty or not have a declaration of a variable on it. The variable is equated to a term:

```pedant
donation_size = 100000
```

On the right is the value for that term.

That value can make up an expression:

```pedant
consumption_possible_by_funds = 
  (1 - percent_of_transfers_invested) 
  * size_of_transfer_per_person 
  / duration_of_initial_consumption
```

If you want, you can break expressions over multiple lines, with the condition
that every line within the expression must be indented.

The available operators are:
 - `*` multiplication
 - `/` division
 - `^` exponentiation
 - `+` addition
 - `-` subtraction

There is also one inbuilt function currently available, the natural logarithm: `ln`. 
Functions are applied by whitespace, like functional languages such as Haskell. 
For instance `ln 5`.

Paranthesis can be used to impact the order of operations. Order of operations
follow BODMAS, as you would expect.

When declaring a value, only values higher up in the file can be referenced.
This ensures there is no circularity in definitions.

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

However, the basics of dimensional checking is that you can declare a unit and assign a unit to a value,
such as

```pedant
unit person
average_household_size = 4.7 person
```

The "unit person" line declares a unit "person" to be used later, which is then
used in the next line

This indicates that average_household_size is in units `person`.

The checker is really simple, it basically follows two main rules:

- When multiplying two values together, we must also multiply the units. Also you divide the units when you divide two values
- You cannot add or subtract two values of different units

So for instance, the following construction is valid:
```pedant
unit bag apple
bag_count = bag
apples_per_bag = apple bag-1
total_apples = apples_per_bag * bag_count
```

as `total_apples` would be of units `apple`.

(Also note that you can put multiple units in the same unit declaration)

But this is invalid:

```pedant
unit bag apple
bag_count = bag
apples_per_bag = apple bag-1
total_apples = apples_per_bag + bag_count
```

As you are adding two values of different units.

This helps you ensure that the math and reasoning behind your calculations are correct.

As above in `bag-1`, a dimension can have a number after it that indicates what the power of that dimension is. In this case, this means `1/bag`, or "per bag". You can also do things like `m2` to mean meters squared.

# More advanced syntax

## Lists

Lists in pedant are represented through standard array syntax:

```pedant
one_to_five = [1, 2, 3, 4, 5]
```

Empty arrays are not allowed.

You can do all operations you can on normal numbers on lists. The operations
work pointwise, so:
```pedant
// add one to each number
two_to_six = one_to_five + 1
// [2, 3, 4, 5, 6]

// add one to each number
two_to_ten = one_to_five * 2
// [2, 4, 6, 8, 10]
```

## Records
A record type is a type that has contains a collection of other different types
associated with different keys. In pedant, they are defined in a similar way
to Haskell

```pedant
unit meters
my_point = { x = 20 meters, y = 25 meters }
my_point_x = my_point.x
my_point_y = my_point.y
```

## Functions
You may define your own custom functions, such as:

```pedant
present_value payment discount duration = 
  payment * (1 -  discount ^ (-duration)) / ln discount
```

In this example, the present_value function has arguments payment, discount and
duration. These arguments are can then be referenced in the function body.

Full recursion is not supported.

Functions in Haskell are all statically typed, except the types of arguments
are inferred from the way that you use them. For instance:

```pedant
addOne x = x + 1
```

is inferred to be a function which takes a dimensionless value x and adds one
to it. It managed to work this out because you are adding 1 to x, because 1 is
dimensionless, and you can only add numbers that are the same units, x must be
dimensionless.

If for instance, the function was:

```pedant
addOneUsd
addOne x = x + 1 usd
```
Then the function can be inferred to take a variable x which is in usd and return
a value in usd.

To represent function type signatures, we write `usd -> usd`. Meaning a function
which takes usd and returns usd.

Functions can be polymorphic, say for instance:

```pedant
multUsd x = x * 2 usd
```

is of type `'a -> 'a usd`. Types like `'a 'b 'c` etc are polymorphic types, meaning
that they could be anything that the user wants to put in them. This says that
the functions adds the usd dimension to whatever dimension `'a` ends up being.


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
unit year
discount_rate = 1.04 ^year-1
```

There are only two valid places you can put a variable with a power dimension, in a logarithm, or the base of an exponential. For instance:

```pedant
value = start_value * (discount_rate ^ (-3 year))
```

In this case, the dimension of the discount_rate and the exponent multiply together, being `year-1` and `year`, so they cancel each other out.


In a logarithm:
```pedant
value = ln(discount_rate)
```
The power dimension simply transforms back into a normal dimension, into `year-1`.

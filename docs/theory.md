# Dimensional Analysis Theory

Dimensional analysis is a fairly simple concept with a very complicated
name. The ideas come from physics, where some calculations are not
physical, mainly because it would be unclear what the units would be of
that calculation. For instance:

You can add two things of the same units:

```pedant
unit meters
length1 = 2 meters
length2 = 4 meters
unknown = time + length
```

Because if you add 2 meters and 4 meters, you end up with 6 meters.
However, adding meters to seconds:

```pedant
unit seconds meters
time = 2 seconds
length = 4 meters
unknown = time + length
```

2 seconds + 4 meters is 6... somethings. We wouldn't really know what unit
to give this 6. As of such, this is not a "physical" calculation, and as
of such you would never find this calculation meaning anything in the real
world.

This is useful! Because it, in a sense, allows us to "Type Check" our
calculations. Where some calculations are invalid, and others are not. This type checking allows you to find missing assumptions and errors within your calculations, as Sam Nolan has done with [GiveDirectly](https://forum.effectivealtruism.org/posts/WmQwtYEajNDuPdyZx/type-checking-givewell-s-givedirectly-cost-effective).

In physics, there are basically two rules in dimensional analysis:

- When you multiply or divide two numbers, you must multiply or divide the units.
- You cannot add, subtract or equate two calculations that are of different units.

This is usually fine for the purposes of physics. But for the purposes of
economics. This leaves some important considerations out. Notably, what
about powers and logarithms?

## An extension of Dimensional Analysis

One very common construction in economics is that of interest, most notably:

(This isn't valid pedant, it won't compile)
```pedant
unit years usd
princpal = 1000 usd
duration = 10 years
interest_rate = 1.05
after_interest = principal * interest_rate ^ duration
```

Normally, when dealing with powers within physics, the general rule is
that the base and the exponent must both be dimensionless. If either of
them are dimensionful, then this is not valid.

However, in this case, we have an exponent and a base that both seem to
have dimensions. Most notably, the exponent, `duration`, is the units `years`, which would violate this restriction. 

What's even more interesting in that the `interest_rate` is that you can also think that maybe `interest_rate` should have a unit. We might say that the interest rate is "5% per year", so clearly it has some sort of unit that depends on years.

The next thing to notice however, is that if `interest_rate` does have a unit, it isn't a normal unit. For instance, we can't add interest rates. a rate of 105% plus another rate of 105% getting to 210% doesn't really make any sense. It does however make sense to multiply the interest rates, for instance 105% times 105% is just the interest rate over two years.

Because of this observation, pedant defines a special type of unit for this case, called a "power unit". A power unit starts with a `^`. For instance, in this case, the unit of `interest_rate` is of `^year-1`.

Power units can be used in only two places, the base of an exponent and inside a logarithm. The exact details on what happens to each of them in those areas are in the [operations table](operations.md).

## The beginnings of a theory of dimensionality
This begs the question, are there any more extensions to dimensional analysis? What is the definition of dimensional analysis? What defines something as physical mathematically?

Our definition of a dimensional quantity is based off the concept of converting
units to other equivalent units. We define a conversion function. A conversion
function simply converts a number from one set of units to an equivalent set of
units.

For instance, if we have 6 meters, and we want to convert this into centimeters,
we have 600 centimeters. The conversion function, knowing the result, it's units
and what units to convert it into, can validly convert from one set to the other.
The conversion function for going from meters to centimeters is simply multiplying
by 100.

The conversion function shouldn't care about how we managed to get this 6 meters,
it shouldn't need to know. For instance:

- 4 meters + 2 meters = 6 meters, and 400 centimeters + 200 centimeters = 600 centimeters
- 5 meters + 1 meter = 6 meters, and 500 centimeters + 100 centimeters = 600 centimeters.

A valid conversion function works no matter how it was created.

Now lets prove that the expression 4 meters + 2 seconds = 6 unknowns Is not dimensional by
this definition. We will us a proof of contradiction.

Assuming the units of 6 does have a conversion function, we should be able to convert
the meters into seconds. Because we constructed our 6 with 4 meters + 2 seconds,
this becomes 400 centimeters + 2 seconds = 402 unknowns.

However, if this 6 unknowns was constructed in a different way, say 5 meters + 1 second = 6 unknowns.
Then the result would be 500 centimeters + 1 second = 501 unknowns.

This means that this conversion function, converting from meters to centimeters,
would give two different answers (and indeed, can give an infinite number of answers)
where these answers depend on information it shouldn't know. Therefore, this function
is not a function, and 4 meters + 2 seconds is not dimensional.

This structure of proof is used to determine whether a calculation is dimensional
or not dimensional. 

## A full formal specification

To do this proof formally, we will have to lay down the proper definitions.

A calculation is defined by:

$$
  f(x_1,x_2, ... x_n, y_1, y_2, ... , y_m)
$$

Where $x_1, x_2, ..., x_n$ are the values in the function, and $y_1, y_2,  ..., y_m$
are the units used in the calculation. For instance, adding two units units that
are the same can be represented as:

$$
  f(x_1, x_2, y_1) = x_1y_1 + x_2y_1
$$

And adding two units that are different can be represented as

$$
  f(x_1, x_2, y_1, y_2) = x_1y_1 + x_2y_2
$$

Normal units are specified by simply multiplying the unit by the value. Dimensionless
constants are represented as just the value with no unit multiplied to it.

We define a *conversion function* $g$ with the following signature:

$$
  g(u, y_1, y_2, ..., y_m, y_1', y_2', ..., y_m')
$$

This conversion function takes a value $u$, and converts the units of the value.
It is given a set of old units, $y_1, y_2, ..., y_m$ and a set of new units to
convert these to $y_1', y_2', ... y_m'$.

The function $g$ is called a conversion function for $f$ if:

$$
    g(f(x_1,x_2, ..., x_n, y_1, y_2, ..., y_m), y_1, y_2, ..., y_m, y_1', y_2', ..., y_m') = f(x_1, x_2, ..., x_n, y_1', y_2', ..., y_m')
$$

For all possible values of $x_i$, $y_i$ and $y_i'$.

For example, for the following function (adding two items of the same units $y_1$).

$$
  f(x_1, x_2, y_1) = x_1y_1 + x_2y_1
$$

The conversion function is:

$$
  g(u, y_1, y_1') = \frac{uy_1'}{y_1}
$$

As

$$
  g(f(x_1, x_2, y_1), y_1, y_1') = f(x_1, x_2, y_1') \\
  \frac{(x_1y_1 + x_2y_1)y_1'}{y_1} = x_1y_1' + x_2y_1' \\
  x_1y_1' + x_2y_1' = x_1y_1' + x_2y_1'
$$

However, for adding two items that are of different units:

$$
  f(x_1, x_2, y_1, y_2) = x_1y_1 + x_2y_2
$$

We can prove the conversion function doesn't exist. Because assuming 
$x_1 = 4$, $x_2 = 2$, $y_1 = y_2 = y_2' = 1$ and $y_1' = 10$.

$$
  g(f(x_1, x_2, y_1, y_2), y_1, y_2, y_1', y_2') = f(x_1, x_2, y_1',y_2') \\
  g(f(4, 2, 1, 1), 1, 1, 10, 1) = f(4, 2, 10, 1) \\
  g(6, 1, 1, 10, 1) = 42
$$

However, if we were to take another set of assumptions, particularly
$x_1 = 5$, $x_2 = 1$, $y_1 = y_2 = y_2' = 1$ and $y_1' = 10$. Then
we would have

$$
  g(f(x_1, x_2, y_1, y_2), y_1, y_2, y_1', y_2') = f(x_1, x_2, y_1',y_2') \\
  g(f(5, 1, 1, 1), 1, 1, 10, 1) = f(5, 1, 10, 1) \\
  g(6, 1, 1, 10, 1) = 51
$$

Therefore we come across a contradiction, as

$$
  g(6, 1, 1, 10, 1) = 42 = 51
$$

So therefore $g$ does not exist.

These types of proofs can be used to show a particular calculation to be dimensionful
or not dimensionful according to the theory.

## New units

In [operations](operations.md), a spreadsheet is provided with all the possible
operations. Each operation is considered invalid if it does not have a conversion
function. This however leaves some interesting units that are not yet included
in this calculation.

### Logarithmic units
The first one, which is not particularly interesting or useful, is that it seems
possible to have the logarithm of a dimensionful quantity:

$$
  f(x_1, y_1) = \ln (x_1y_1)
$$

With conversion function

$$
  g(u, y_1, y_1') = \ln (\frac{y_1'e^u}{y_1})
$$

This unit is particularly useless as the only operations that seems to be valid
on this unit are:

1. `e^x`, to transform it back into a normal unit
2. Adding and substracting it from dimensionless quantities to get back the logarthmic units
3. Adding it to another logarthmic unit to multiply the units.

Because this unit has so little interesting properties, doesn't represent any
useful notion, and breaks many of the assumptions that one makes when thinking
about units. I have decided not to include this.

### Dimensionful constants

Dimensionful constants however, are an interesting concept that I have so far
seen two use cases of. They are however very confusing.

The first use case is a strange problem that shows up with interest rates. When
we normally talk about interest rates, we might say that the rate is `4%`. For
instance, in GiveWell's Cost Effectiveness Analysis, the Cost Effectiveness Analysis
the discount rate is `4%`, and is written down as `0.04`.

However, in pedant, you'll see that it has been written down as `1.04 years-1`.
Why? Because to write it down as `0.04` it would be of a units that can't currently
be represented in pedant.

Let's take a look at the present value formula, one used pervasively in GiveWell's
Cost Effectiveness Analysis:

$$
  pv(p,r,d) = p\frac{1- (1 + r)^{-d}}{\ln (1 + r)}
$$

Where $p$ is the payment, $r$ is the discount rate, and $d$ is the duration.
You'll notice that every time $r$ is used, 1 is added to it. This is because, in
the terms of pedant, it becomes a power unit when 1 is added to it.

This means that `0.04` is in a sense the unit `(^years-1) - 1`. And you must
add 1 to make it a proper `^years-1`.

This is defined in our theory, where the calculation is:

$$
  f(x_1, y_1) = x_1^{\frac{1}{y_1}} - 1
$$

Which has the conversion function

$$
  g(u, y_1, y_1') = (u + 1)^{\frac{y_1}{y_1'}} - 1
$$

A second use case for dimensionful constants came up when writing very fine units
for GiveDirectly's cost effectiveness analysis.

A core element of the analysis is the idea that a portion of the payments are directed
towards investments, and the other half are immediately consumed. As much as this
seems intuitive, it's difficult to write in pedant.

Say you have much finer units than we might be use to, and the portion directed
towards investments are in units `usdinvested usdreceived-1`, meaning "Amount
of money invested of the amount received", then you would intuitively define
Amount of money consumed to be `usdconsumed usdreceived-1`. However, the issue
comes when these are related. For instance, the amount of transfers invested in
the analysis is `39%`, or in pedant `0.39 usdinvested usdrecieved-1`, but to 
calculate the amount consumed, then you must go `1 - 0.39 usdinvested usdrecieved-1`.
This fails because you can't subtract a dimensionless quantity from a dimensionful
one.

One possible way to fix this issue is to add in a special type of unit aliasing,
we know that `usdinvested usdreceived-1 + usdconsumed usdreceived-1 = 1`, so
what if we were to define

`unit alias usdconsumed usdreceived-1 = 1 - usdinvested usdreceived-1`

This tells pedant that when you have 1 - `usdinvested usdreceived-1` you get
`usdconsumed usdreceived-1`. However, in doing this, what shows up is yet another
dimensionful constant (the 1 in the definition).

I like adding this definition into the pedant file, as it explicitly represents
an assumption that I would like to be picked up and tracked.

I don't currently fully understand dimensionful constants, and what they mean,
and if they have already been investigated.

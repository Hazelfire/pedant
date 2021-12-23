# Frequently Asked Questions

Here's a list of commonly confusing elements about pedant. We're working to make
the language to be as understandable as possible, but there are some compromises
made that you may stumble across.

## Units and powers

A squared unit, such as 1 second squared, is represented as `1 seconds2`.

if you want to find the area of a squared paddock, you must cannot do 

```pedant
width = 5 meters
area = width^2
```

You must multiply it with itself, like:

```pedant
width = 5 meters
area = width * width
```

To understand why, and to see a discussion about this, see [issue #5](https://github.com/Hazelfire/pedant/issues/5)

## Functions

Functions currently work the same way that 

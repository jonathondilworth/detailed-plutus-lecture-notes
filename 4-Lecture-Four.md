# Lecture Four

### 1. Introduction

*In Progress*

### 2. An Advanced Haskell Primer

### 2.1 Referential Transparency

The following is an extract from a very well known Haskell tutorial / book:

> In purely functional programming you don't tell the computer what to do as such but rather you tell it what stuff is. The factorial of a number is the product of all the numbers from 1 to that number, the sum of a list of numbers is the first number plus the sum of all the other numbers, and so on. You express that in the form of functions. You also can't set a variable to something and then set it to something else later. If you say that a is 5, you can't say it's something else later because you just said it was 5. What are you, some kind of liar? So in purely functional languages, a function has no side-effects. The only thing a function can do is calculate something and return it as a result. At first, this seems kind of limiting but it actually has some very nice consequences: if a function is called twice with the same parameters, it's guaranteed to return the same result. That's called referential transparency and not only does it allow the compiler to reason about the program's behavior, but it also allows you to easily deduce (and even prove) that a function is correct and then build more complex functions by gluing simple functions together. [[1]](#1)

### References

<a href="#1" id="1">1.</a> Lipovaca, M., 2011. Learn you a haskell for great good!: a beginner's guide. no starch press.


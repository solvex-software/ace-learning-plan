# Ace Interview Prep - Haskell Tutorial #










### Rough draft template: ###


**Introduction**

Modern OO is copying FP because it’s better, but OO compilers can’t handle its requirements
Why does FP matter? (Immutability, generative testing, pure functions, laziness, etc.)

**Lesson 0 - Think like a Functional Programmer**

How to approach problem solving - every problem is the same
How to build an application / what is a functional application?
Different types of applications have different requirements for safety. FP is hands down better for risky codebases.


**Lesson 1 - Types & Domain Theory**


**Lesson 2 - Immutability Changes Everything**

Currying, pure evaluation forced, benefits of this


**Lesson 3 - It's all Patterns**

Pattern matching, guards, and if-else


**Lesson 4 - Working with Lists and Sets**

List comprehensions, spines, and folds


**Lesson 5 - Combine your Functions**

Common combinators


**Lesson 6 - Building Something Real**

Components of a common web/mobile app
Different types of applications (embedded, realtime, microservices, APIs, etc)


**Lesson 7 - Building Your First API**


**Lesson 8 - Databases and FRMs**


**Lesson 9 - Cross-Platform Web/Mobile Development**


**Lesson 10 - Generative Property Testing**


**Lesson 11 - Deterministic Environments using Nix**














Brainstorming points:

Modern OO is copying FP because it’s better, but OO compilers can’t handle its requirements
Why does FP matter? (Immutability, generative testing, pure functions, laziness, etc.)
Intro: Type Theory and its motivations for programming. Monoids, Functors, Monads, Semigroups? Demystify from beginning that these are mathematical terms for combination, generic application, generic context, and complex sets, and that we just meed to remember the names to pay respect to the mathematicians that came up with the concepts)
Compilation as the first step in your test plan (explain how a modern FP test plan looks like compared to the traditional TDD approach used in procedural and OO
Domain modelling , type classes and type families
Pattern matching, guards, if-else
List comprehensions, spines, and folds
Apis in Haskell (Servant)
Databases in Haskell (Persistent, FRM vs ORM)
Integrations & nixpkgs
Cross platform development with Obelisk & Reflex-FRP

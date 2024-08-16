# Haskell Tutorial #


# Introduction



# Lesson 0 - Think Like a Functional Programmer

## Introduction

How you approach problem-solving as a programmer can dramatically affect your outcomes. In the world of functional programming (FP), the way we think about problems is fundamentally different. The essence of FP is to break down problems into their most basic elements, using mathematical principles to guide us toward clean, reliable solutions. Every problem, no matter how complex, can be simplified when we apply first principles analysis.

## How to Approach Problem Solving: Every Problem is the Same
The best way to tackle problem-solving is by using **first principles analysis**. This means breaking down a problem into its most basic, fundamental truths, and building up from there. By modeling the ground-roots information about the space the problem exists within, we can greatly increase our understanding and, as a result, simplify our implementation. This ensures that what we build not only works but is also maintainable and scalable.

### Example: First Principles in Action
Let's start with a simple problem. Imagine you're building a system to handle payments. What are the first principles here?

1. **Identify the Inputs**: We start by identifying the basic inputs to our problem. In this case, our inputs might be a cart of items, each with an ID, name, and price, and a payment method, which could be an encrypted reference to the user's payment details on a platform like Stripe or PayPal.

2. **Define the Outputs**: Next, we define the outputs. What needs to happen? We need to process a payment, confirm that it went through, handle any failures, and then create an order and track its fulfillment.

By breaking it down like this, we’re not jumping straight to a solution. Instead, we’re mapping out the entire problem space. The solution, more often than not, becomes obvious once we’ve done this.

```haskell
-- Define the structure of an item in the cart
data Item = Item { itemId :: Int, itemName :: String, itemPrice :: Double }

-- Define the cart
data Cart = Cart { items :: [Item] }

-- Example payment information
data PaymentMethod = Stripe String | PayPal String

-- Function to calculate total cost
calculateTotal :: Cart -> Double
calculateTotal (Cart items) = sum [itemPrice item | item <- items]

-- Function to process payment
processPayment :: Cart -> PaymentMethod -> Either String Order
processPayment cart paymentMethod =
  let total = calculateTotal cart
  in case paymentMethod of
       Stripe token -> -- Process Stripe payment here
       PayPal account -> -- Process PayPal payment here
```

### Taking Baby Steps
When we have a model of the problem space, we can take baby steps toward solving it. First, we identify what the inputs to our problem are. If the problem is that we need a way to handle payments for our users, the inputs might be the cart of items and the payment method.

For instance, in our payment system, the inputs might include the list of items in the cart, where each item has an ID, name, and price. The solution isn't something we think about yet; instead, we focus on understanding what we have (inputs) and what we want (outputs).

Next, we define what the outputs of our problem space are. What needs to happen? We need to process a payment, ensure it goes through, and handle cases where it doesn’t. We also need to create an order and track its fulfillment.

Here’s how we might model this at a high level using domain modeling:

```haskell
-- Define the output data structures
data Order = Order { orderId :: Int, orderTotal :: Double, orderStatus :: String }

-- Example of a transformation function
processOrder :: Cart -> PaymentMethod -> Either String Order
processOrder cart paymentMethod = 
  -- Assume processPayment returns Either String Order
  case processPayment cart paymentMethod of
    Right order -> Right order
    Left errMsg -> Left errMsg
```

This simple model allows us to start reasoning about our solution. We're not rushing to code; we're taking the time to ensure our understanding is correct.

### Stub Out the Transformation Functions
Now that we have a clear model, we can create the transformation functions that will make our application work. Start with the type signatures. Why? Because type signatures are like the scaffolding of your solution—they define the shape and structure of what you’re building.

```haskell
-- Type signatures help us think about the problem
calculateTotal :: Cart -> Double
processPayment :: Cart -> PaymentMethod -> Either String Order
```

With these type signatures, you can start reasoning about what each function needs to do. Notice how we're breaking everything down into small, manageable pieces.

### Small Components, Big Impact
When creating functions, it’s best to think about them in small, reusable components. The less they do, the more useful they are across different parts of your application. Haskell excels at allowing us to compose functions together in meaningful ways, and as you progress through this guide, you'll learn a bunch of neat ways to do this.

Remember, Haskell is actually very simple at its core. Complexity arises from the many ways you can compose and modify functions, but if you understand the basics, you'll be able to build anything.

```haskell
-- Compose small functions to achieve bigger tasks
applyDiscount :: Double -> Double -> Double
applyDiscount discount total = total * (1 - discount)

totalWithDiscount :: Double -> Cart -> Double
totalWithDiscount discount = applyDiscount discount . calculateTotal
```

### Functional Programming Mindset

You might be thinking, "This all sounds great, but how does it help me build a real application?" Good question. The FP mindset is all about leveraging these small, composable functions to build up complex behavior in a controlled, predictable way. It’s like building a house out of LEGO bricks—each brick is simple, but the combinations are limitless.

Take the time to understand the function signatures of unfamiliar functions. They’re your roadmap, guiding you through the complex landscape of Haskell. Approach each function one property at a time, and before you know it, you'll be solving problems with elegance and ease.

### Intro: Type Theory and its Motivations for Programming

Before we dive deeper, let’s touch on something that often confuses newcomers: terms like Monoids, Functors, Monads, and Semigroups. These might sound intimidating, but they’re just mathematical concepts that we use to structure our programs in a more reliable and predictable way.

- **Monoids**: Think of Monoids as a way to combine things. If you’ve ever added numbers or concatenated strings, you’ve already used a Monoid. It’s just a way of saying, "Here’s how you combine two things to get another thing of the same type."
  
- **Functors**: Functors are about applying a function to something within a context. For example, if you have a value wrapped in a `Maybe`, a Functor lets you apply a function to that value without having to unwrap it first.

- **Monads**: Monads are about chaining operations that are context-aware. If you’ve ever worked with `IO` in Haskell, you’ve used a Monad. They allow us to sequence operations that involve side effects in a controlled manner.

- **Semigroups**: These are like Monoids, but without the need for an identity element. They’re still about combining things, but in a slightly less restrictive way.

Don’t worry if these concepts don’t fully click right away. We’ll be exploring them in more detail as we progress. For now, just remember that these are mathematical terms that help us reason about how to combine, apply, and manage contexts in our programs. And yes, the names are a nod of respect to the mathematicians who developed these ideas.

Remember, Haskell is actually very simple at its core. Complexity arises from the many ways you can compose and modify functions, but if you understand the basics, you'll be able to build anything.


## How to Build an Application: What is a Functional Application?
What does it mean to build a functional application? In essence, an application is a sequence of instructions that runs along a timeline. But in functional programming, we treat this timeline very differently from traditional procedural or object-oriented approaches.

### Traditional vs. Functional Approaches
In traditional programming, especially in procedural and object-oriented paradigms, applications often involve hidden states and side effects. This can lead to a slew of issues during runtime that are difficult to track down and fix.

Functional programming, on the other hand, abides by the laws of mathematics. Your entire application is like one massive math equation, where everything is pure and predictable. This might seem like a hassle at first, but trust me, the benefits far outweigh the initial learning curve.

```haskell
-- Pure function: always returns the same output for the same input
add :: Int -> Int -> Int
add x y = x + y

-- Impure function: might return different outputs depending on external factors
getCurrentTimeAndAdd :: Int -> IO Int
getCurrentTimeAndAdd x = do
    currentTime <- getCurrentTime
    return (x + round currentTime)
```

Notice how the pure function add will always give the same result for the same inputs, while getCurrentTimeAndAdd depends on an external factor, making it impure. This predictability is what makes functional programming so powerful.

### The Identity of a Functional Program
Here’s a fun thought experiment: imagine your entire application as one massive mathematical equation, filling a chalkboard. When it’s done, it compiles down to a single function. That function, given the same input, will always produce the same output. That’s the identity of a functional program—it’s self-contained and predictable.

For those who have slogged through procedural or object-oriented codebases, particularly in dynamically typed languages, you know how frustrating it can be when your program behaves unpredictably. In FP, you can sidestep many of these headaches. If you’re just starting out now, count yourself lucky!

### Functional Programming in the Real World
FP isn’t just for academics and hobbyists. It’s a powerful tool for building real-world applications, especially in industries where precision, reliability, and scalability are paramount. Let’s dive into why FP is hands down better for risky codebases.

## Different Types of Applications Have Different Requirements for Safety
Not all applications are created equal. Some need more safety guarantees than others. Functional programming provides these guarantees, making it an excellent choice for industries where the cost of failure is high—like healthcare, finance, aerospace, and more.

### Why FP is Ideal for High-Stakes Applications
FP gives us a special guarantee: over the entire program flow of our application, we will not receive an unexpected result. This is because the pure nature of the compiler forces us to turn exceptions into expectations.

Let’s break that down with an example.

```haskell
-- Handling risky operations with Either
processTrade :: Trade -> Either String Confirmation
processTrade trade =
    if tradeAmount trade > 0 then Right Confirmation
    else Left "Invalid trade amount"
```

In this snippet, we’re processing a trade. Notice how we’re explicitly handling the possibility of an invalid trade. FP forces us to think about these edge cases upfront, making our code safer and more reliable.

### FP in Action: Case Studies
Let’s take a look at some real-world examples where FP is making a difference:

1. **Finance**: Major financial institutions are using Haskell to build trading systems. Why? Because when billions of dollars are on the line, you can’t afford unexpected bugs.

2. **Aerospace**: NASA uses functional programming principles to ensure that their software systems are fault-tolerant and reliable. Imagine if a bug in the code caused a spacecraft to malfunction—FP helps prevent that.

3. **Healthcare**: In the medical field, software bugs can literally be a matter of life and death. Functional programming’s strong safety guarantees make it a natural fit.

## Conclusion
You’ve made it through the first lesson, and hopefully, you’re starting to see the power of thinking like a functional programmer. By focusing on first principles, breaking down problems into small components, and embracing the mathematical nature of FP, you’re setting yourself up for success.

## Exercises
1. **First Principles Analysis**: Pick a problem you’ve encountered recently and break it down using first principles. Write a Haskell function that models your solution.

2. **Domain Modeling**: Choose a domain (like an e-commerce platform or library system) and create stub functions with type signatures that handle the main operations.

3. **Pure vs. Impure Functions**: Take a simple Haskell program and identify which functions are pure and which are impure. Rewrite the impure ones to be pure where possible.

Remember, this is just the beginning. As we continue, you’ll learn more about how to apply these principles to build robust, scalable, and maintainable applications.






### Rough draft template: ###


**Introduction**

Modern OO is copying FP because it’s better, but OO compilers can’t handle its requirements
its growing, microsoft is adapting it, hiring simon peyton jones. Tim Sweeny with Simon on Verse. Tesla, SpaceX, & Neuralink using Haskell. <gpt: Find examples.>
Why does FP matter? (Immutability, generative testing, pure functions, laziness, etc.)


<Read this to set up your development environment and get used to using the interactive REPL (GHCI)> # todo: link to document on this


**Lesson 0 - Think like a Functional Programmer**

How to approach problem solving - every problem is the same
- the best way to go about problem solving is using first principles analysis. By modelling the ground-roots information about the space the problem exists within, we can greatly increase our understanding of the problem and thus simplify our implementation and ensure it works.
  - <gpt: example of this in haskell code>
- when we have a model of the problem space, we can take baby-steps towards solving it.
- First, we identify what the inputs to our problem are. If the problem is that we need a way to handle payments for our users, the inputs to this might be the cart of items, where each item has an item id and name. It might also include an encrypted reference to the users payment details on a platform like Stripe or PayPal, or just the connection to Stripe itself. We have to define what information we need to solve the problem, before we can start thinking about the solution. The solution is the last thing we think about, and it's often obvious by the time we implement it.
- We then define what the outputs of our problem space are. What are the things that need to happen? Well, we need to allow someone to make a payment, we need to be sure that the payment went through and handle cases where it doesn't, and then we need to ultimately create an order and track it's fulfillment. We can model this like so:
   - <gpt: use domain modelling in haskell to map this out at a high level>
- Finally, we create the transformation functions which actually make your application work. Starting with type signatures, we can start to reason about what each function will need to do.
    - <gpt: create stubbed transformation functions with type signatures to fulfil this example>
- When creating functions, it's best for us to think about them in small components. Try to make their responsiblities very minimal and reusable. Haskell gives us a lot of ways to compose functions together, and as you go through this program, you will learn a bunch of neat ways to do this. Haskell is actually very simple, but it becomes complex due to the number of ways you can modify it. Always look up the function signatures of functions you are unfamiliar about and try to reason about them one property at a time. 
- <gpt: steal ideas from "think like a programmer" book and round this out with functional programming mindset.>
  
How to build an application / what is a functional application?
- An application is a sequence of instructions that run along a timeline. <gpt: dive into this more in detail>
- Traditional procedural and object-oriented approaches that dominate the industry have inherent issues where the state is hidden from the user at runtime, causing a sleiu of issues that the industry attempts to wrangle during runtime.
- With functional programming, our entire application must abide by the laws of mathematics and must compile down to a single function. When it's done, it would look like one massive math equation that fills a chalk-board. The important thing to note is that the identity of a functional program is itself, meaning that everything within the functional application is pure and will always produce the same result. (Even if it has side effects that you can't control). For those who have done a fair amount of programming in procedural and object-oriented languages, particularly dynamically typed ones, know how much of a pain this can be. If you're just starting out now, be thankful you don't have to deal with any of that! A little more effort up-front will make us far better programmers than our industry competition.


Different types of applications have different requirements for safety. FP is hands down better for risky codebases.
- As I alluded to above, FP gives us a special guarantee that over the entire program-flow of our application, we will not receive an unexpected result. This is because the pure nature of the compiler forces us to turn exceptions into expectations.
  - <gpt: example of above>
- This makes it especially powerful for software applications that require precision, reliability, and scalability with the cost of either human life or significant amounts of capital; such as those found in the medical, industrial, finance, agriculture, defence, and space industries.
- <gpt: reasons why FP is being used by these types of industries, case studies, etc.>


**Lesson 1 - Types & Domain Theory**


**Lesson 2 - Immutability Changes Everything**

Currying, pure evaluation forced, benefits of this

- metaphor of state immutability with the bucket ***

**Lesson 3 - It's all Patterns**

Pattern matching, guards, and if-else


**Lesson 4 - Working with Lists and Sets**

List comprehensions, spines, and folds


**Lesson 5 - String Parsing**
No Regex, use Parsec through Parser monad
Talk a bit about monads here, and link to more comprehensive document

**Lesson 6 - Combine your Functions**
Common combinators


**Lesson 7 - Building Something Real**

Components of a common web/mobile app
Different types of applications (embedded, realtime, microservices, APIs, etc)


**Lesson 8 - Building Your First API**


**Lesson 9 - Databases and FRMs**


**Lesson 10 - Cross-Platform Web/Mobile Development**


**Lesson 11 - Generative Property Testing**


**Lesson 12 - Deterministic Environments using Nix**














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

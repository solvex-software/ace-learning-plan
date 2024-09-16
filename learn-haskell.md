# Haskell Tutorial #

# Introduction

## Why Learn Haskell? The Rise of Functional Programming

In recent years, there’s been a noticeable shift in the programming world. Modern Object-Oriented (OO) languages are increasingly borrowing concepts from Functional Programming (FP). Why? Because FP has proven to be a better way to build reliable, maintainable, and scalable software. But here’s the catch: while OO languages like C# are trying to adapt FP concepts, they often fall short because their compilers and runtime environments weren’t designed with these paradigms in mind.

This isn’t just a theoretical shift—it’s happening at the highest levels of industry. Microsoft, for example, has hired Simon Peyton Jones, one of the creators of Haskell, to help guide their functional programming efforts. Tim Sweeney, the founder of Epic Games, is working with Simon on a new language called Verse, which brings FP concepts into game development. Meanwhile, companies like Tesla, SpaceX, and Neuralink are using Haskell to solve some of the most complex and high-stakes problems in the world.

## Why not go with the status quo, Object-Oriented Programming?

While these languages are very popular, in practice they are a nightmare for large projects. Why? Imagine you have a huge bin of LEGOs. You decide to recruit 4 friends to helpyou with an ambitious project: building a robot! There are infinitely many ways to put together pieces of lego, as well as infinite ways this project can go right or wrong. While there are many outcomes we can definitely know the characteristics of a successful project vs an unsuccessful one.

### Unsuccessful Characteristics
1. The pieces may work properly on their own but not altogether
2. There may be a faulty component
3. Failures in connected components cause failures in other components
4. One or many components fail when facing some obstacle, such as a bump in the floor, that the robot cannot handle successfully.

### Successful Characteristics 

1. For any set of inputs in an environment it is designed for, the robot as a whole can successfully maneuver 
2. It can perform all the cool tricks you and your amigos designed it to do!
3. And hopefully, you also enjoyed the project and watching it in action

If you were to all take on a different component of the body independent of each other you would likely run into plenty of integration issues. For example, the legs may receive power in a different manner than the arms. There's also the balance and coordination of the robot: when it comes to a bump how does it know to position its arms given the movement of the legs. Are the legs and feet to scale to support the weight of the body? 

Fast forward a few weeks and your new challenge is how to write an adapter, instead of connecting the arms to the torso you connect the arm to the adapter which is connected to the torso. But you still are up against the same original challenges like how do you make sure it balances and now you have much more weight to consider. At this point you and the amigos are much less happy as some of you think "Maybe we should just start fresh" and others think "I am not throwing away 4 weeks of work! I will make an adapter for the adapter if I have to". 

How did we get here? 

You would have been successful if from the start you created pieces which were 100% perfect and did everything they need to and nothing more. 

That might sound difficult but this is the essence of beautiful engineering, math and design. Addition is built on counting which allows us to define multiplication. Similarly if we started with systems that can be fully defined in terms of all inputs and outputs, then we would know that this independent system would just be like another piece of LEGO that we can easily combine with other LEGOs and other well-built systems. 

Ace is a platform which is built entirely using functional programming with the Haskell language. It's allowed for automatic database migration without fear, in-depth video processing through WebRTC, and over 150 unique features. We can easily add new features without worry of breaking old ones. Even when we need to change old features, we can do so easily because of the type awareness of the Haskell compiler. 

Our team has also been in many projects written in object-oriented languages, which initially seemed quick with the use of libraries but then spent even longer working to adapt the library to some other component and spaghetti code that no one has been willing to touch for years. 

The difference in the outcomes these projects have in practice simply comes down to the compiler. As we will see throughout the Ace program, it comes down to the Haskell compiler disallowing numerous problematic ways to code, and the thinking patterns that result from working with such a strongly typed compiler.

After using Haskell, you will have a mental framework to discover the best solution to any given problem. You will also know the difference between strong and weak coding patterns and know whether or not you can trust a function. This mental framework is one that can be applied not only to amazing languages like Haskell and other functional strongly-typed languages but even to languages which don't stop you from writing bad code. 

## Why Does Functional Programming Matter?

Before we dive into Haskell, let’s talk about why FP is gaining so much traction. The core principles of FP—immutability, pure functions, laziness, and generative testing—offer solutions to many of the challenges that OO programming struggles with.

- **Immutability**: In FP, data is immutable by default. This means that once you create a value, it cannot be changed. Compare this to C#, where objects can often change state in unpredictable ways, leading to bugs that are hard to track down. In Haskell, because data doesn’t change, you can reason about your code more easily, leading to fewer bugs and more robust software.

- **Pure Functions**: A pure function is one that, given the same inputs, always produces the same output and has no side effects. This is a cornerstone of FP and contrasts sharply with the OO approach, where methods often modify the state of objects or interact with global state. In C#, methods often have side effects, which can make programs harder to understand and maintain. In Haskell, the use of pure functions means that your code is more predictable and easier to test.

- **Laziness**: Haskell is a lazy language, meaning that it doesn’t evaluate expressions until absolutely necessary. This can lead to performance improvements and allows for the creation of more abstract and reusable code. In contrast, C# is an eager language, evaluating expressions as soon as they are encountered, which can sometimes lead to inefficiencies.

- **Generative Testing**: FP languages often emphasize generative testing, where the program itself generates test cases. This is in contrast to traditional unit testing in OO languages like C#, where developers manually write test cases. Haskell’s QuickCheck library, for example, allows you to describe properties that your functions should satisfy, and then automatically generates test cases to verify those properties. This approach can uncover edge cases that you might never think to test manually.

## A Side-by-Side Comparison: C# (OOP) vs. Haskell (FP)

Let’s look at some specific examples to see how these differences play out in practice.

### Example 1: Immutability

In C#, immutability is not the default. Here’s how you might write a class that represents a point in 2D space:

```csharp
public class Point {
    public int X { get; set; }
    public int Y { get; set; }

    public Point(int x, int y) {
        X = x;
        Y = y;
    }

    public void Move(int deltaX, int deltaY) {
        X += deltaX;
        Y += deltaY;
    }
}
```

This class allows you to change the position of the point after it’s created. This mutable state can lead to bugs, especially in larger systems where the state might be modified by different parts of the program at different times.

Now, here’s how you might write a similar structure in Haskell:

```haskell
data Point = Point { x :: Int, y :: Int }

move :: Point -> Int -> Int -> Point
move (Point x y) deltaX deltaY = Point (x + deltaX) (y + deltaY)
```

In Haskell, Point is immutable. The move function doesn’t change the original point—it returns a new point with the updated coordinates. This immutability makes the code easier to reason about, as you never have to worry about the state of a Point changing unexpectedly.

### Example 2: Pure Functions

Consider a method in C# that reads a file and processes its contents:

```csharp
public string ProcessFile(string filePath) {
    string content = File.ReadAllText(filePath);
    return content.ToUpper();
}
```

This method is not pure. It has a side effect (reading a file from the disk) and its output depends on the state of the file system.

In Haskell, you would separate the side effect (reading the file) from the pure function (processing the contents):

```haskell
processFile :: String -> IO String
processFile filePath = do
    content <- readFile filePath
    return (map toUpper content)
```

Here, readFile is an IO action that performs the side effect of reading the file, but map toUpper is a pure function that transforms the string. This separation makes the code easier to test and reason about.

### Example 3: Laziness

In C#, if you create a list of numbers, the list is fully evaluated as soon as it’s created:

```csharp
var numbers = new List<int> { 1, 2, 3, 4, 5 };
```

If this list is large, it might consume a significant amount of memory, even if you only need to process a few elements.

In Haskell, lists are lazy by default:

```haskell
numbers = [1..]
```

This creates an infinite list of numbers, but it’s not evaluated until you actually need the elements. You can then take just the first few elements without evaluating the entire list:

```haskell
take 5 numbers  -- [1, 2, 3, 4, 5]
```

This laziness allows you to work with potentially infinite data structures in a way that’s both memory-efficient and conceptually elegant.

## The Bottom Line: Why You Should Learn Haskell

The industry is moving towards functional programming for good reason. As software systems become more complex, and as AI developers become more prevalent, the advantages of FP (immutability, pure functions, laziness, and generative testing) are becoming increasingly clear. While OO languages like C# are trying to incorporate these concepts, they often do so in a way that feels bolted-on rather than native. Haskell, on the other hand, was designed from the ground up with these principles in mind.

By learning Haskell, you’re not just learning a new language. You’re adopting a new way of thinking about programming. This shift in mindset will make you a better developer, no matter what language you ultimately use. Whether you’re working on high-stakes projects at a company like Tesla, developing the next big game at Epic, or simply trying to write better, more reliable code, the skills you gain from learning Haskell will serve you well.

So let’s dive in and start learning Haskell. You’re about to unlock a powerful new approach to programming that will change the way you think about code forever.


# Lesson 0 - Think Like a Functional Programmer

## Introduction
Before diving into Haskell, we need to shift our mindset to think like a functional programmer. This mindset is different from what you might be used to if you come from a procedural or object-oriented programming background. How you approach problem-solving as a programmer can dramatically affect your outcomes. In the world of functional programming (FP), the way we think about problems is fundamentally different. The essence of FP is to break down problems into their most basic elements, using mathematical principles to guide us toward clean, reliable solutions. Every problem, no matter how complex, can be simplified when we apply first principles analysis.

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

Once we've modeled the problem space by identifying the inputs and outputs, the next step is to actually solve the problem. This is where the concept of **taking baby steps** comes into play. The idea is to break down the problem into the smallest, most consumable pieces you can think of and then write the code that performs those transformations. It’s about constantly asking yourself: 

- Do I understand what needs to be done next?
- Can this be broken down further?
- Is there any domain knowledge missing that I need to understand before proceeding?

By approaching a problem in this way, you ensure that you never get overwhelmed by complexity. Each step is simple, understandable, and can be implemented without too much cognitive overhead.

#### Applying Baby Steps to the Payment Example

Let’s go back to our payment processing example. We’ve already identified the inputs (the cart of items and the payment method) and the outputs (a successful payment and an order). Now we need to figure out how to go from input to output, one small step at a time.

**Step 1: Calculate the Total Cost**

The first transformation we need is to calculate the total cost of the items in the cart. This is a simple, isolated task that can be easily understood. Let’s break it down:

- **Understand the task**: We need to sum up the price of each item in the cart, taking into account the quantity of each item.
- **Missing knowledge**: None, this is straightforward.
- **Write the function**: 

```haskell
calculateTotal :: Cart -> Double
calculateTotal (Cart items) = sum [itemPrice item * fromIntegral (itemQuantity item) | item <- items]
```

Here, the calculateTotal function is doing just one thing: summing up the total cost of all items in the cart. It’s a single, small step, but it’s essential to the overall solution.

**Step 2: Process the Payment**

Now that we have the total cost, the next step is to process the payment. This is a bit more complex, so let’s break it down further:

- **Understand the task**: We need to use the total cost and the payment method to attempt a transaction.
- **Missing knowledge**: How does the specific payment method work? Do we need to handle different payment methods differently?
- **Write the function**:

```haskell
processPayment :: Double -> PaymentMethod -> Either String PaymentResult
processPayment total (Stripe token) = processStripePayment total token
processPayment total (PayPal account) = processPayPalPayment total account
```

In this step, we’ve broken down the task into the act of processing a payment using either Stripe or PayPal. Notice how we didn’t try to solve everything at once. Instead, we created a function that delegates the actual payment processing to other functions (processStripePayment and processPayPalPayment), which might look something like this:

```haskell
processStripePayment :: Double -> String -> Either String PaymentResult
processStripePayment total token = -- logic to interact with Stripe's API

processPayPalPayment :: Double -> String -> Either String PaymentResult
processPayPalPayment total account = -- logic to interact with PayPal's API
```

**Step 3: Generate an Order**

After processing the payment, we need to generate an order. Again, let’s break it down:

- **Understand the task**: We need to create an order record that includes details like the items purchased, the total cost, and the status of the payment.
- **Missing knowledge**: What information needs to be included in the order? How should we handle failed payments?
- **Write the function**:

```haskell
generateOrder :: Cart -> PaymentResult -> Order
generateOrder cart paymentResult = Order {
    orderItems = items cart,
    orderTotal = calculateTotal cart,
    orderStatus = if paymentSucceeded paymentResult then "Confirmed" else "Failed"
}
```

Here, generateOrder takes the cart and the result of the payment process to create an order. Notice how the orderStatus depends on whether the payment succeeded or failed, which is determined by inspecting the PaymentResult.

**The Thinking Process: Breaking Down a Problem into Steps**

The key to successfully applying this “baby steps” approach is to always keep breaking the problem down until you reach a point where each step is small enough that you can confidently implement it. If you encounter something you don’t fully understand, that’s a signal to stop and dig deeper into that specific part of the problem before proceeding.

For example, if you were unsure how to interact with Stripe’s API, you’d break that down further:

- What API endpoints do you need to call?
- What authentication does Stripe require?
- What does a successful or failed transaction response look like?

Each of these questions can lead to a smaller, more manageable task that you can then solve individually.

In the end, the entire payment processing problem is solved by implementing a series of small, well-defined functions, each responsible for a specific aspect of the overall process. By focusing on one tiny piece at a time, you avoid getting overwhelmed and ensure that each part of your program is understandable, testable, and reliable.

This is the essence of functional programming: breaking problems down into manageable, bite-sized pieces, writing pure functions to handle those pieces, and then composing them together to solve the larger problem. With this approach, you can tackle even the most complex programming challenges with confidence.

### Small Components, Big Impact
When creating functions, it’s best to think about them in small, reusable components. The less they do, the more useful they are across different parts of your application. Haskell excels at allowing us to compose functions together in meaningful ways, and as you progress through this guide, you'll learn a bunch of neat ways to do this.

```haskell
-- Compose small functions to achieve bigger tasks
applyDiscount :: Double -> Double -> Double
applyDiscount discount total = total * (1 - discount)

totalWithDiscount :: Double -> Cart -> Double
totalWithDiscount discount = applyDiscount discount . calculateTotal
```

Remember, Haskell is actually very simple at its core. Complexity arises from the many ways you can compose and modify functions, but if you understand the basics, you'll be able to build anything.

### Functional Programming Mindset

You might be thinking, "This all sounds great, but how does it help me build a real application?" Good question. The FP mindset is all about leveraging these small, composable functions to build up complex behavior in a controlled, predictable way. It’s like building a house out of LEGO bricks—each brick is simple, but the combinations are limitless.

Take the time to understand the function signatures of unfamiliar functions. They’re your roadmap, guiding you through the complex landscape of Haskell. Approach each function one property at a time, and before you know it, you'll be solving problems with elegance and ease.

### Demystifying Type Theory and its Motivations for Functional Programming

Before we dive deeper, let’s touch on something that often confuses newcomers: terms like Monoids, Functors, Monads, and Semigroups. These might sound intimidating, but they’re just mathematical concepts that we use to structure our programs in a more reliable and predictable way.

- **Monoids**: Think of Monoids as a way to combine things. If you’ve ever added numbers or concatenated strings, you’ve already used a Monoid. It’s just a way of saying, "Here’s how you combine two things to get another thing of the same type."
  
- **Functors**: Functors are about applying a function to something within a context. For example, if you have a value wrapped in a `Maybe`, a Functor lets you apply a function to that value without having to unwrap it first.

- **Monads**: Monads are about chaining operations that are context-aware. If you’ve ever worked with `IO` in Haskell, you’ve used a Monad. They allow us to sequence operations that involve side effects in a controlled manner.

- **Semigroups**: These are like Monoids, but without the need for an identity element. They’re still about combining things, but in a slightly less restrictive way.

Don’t worry if these concepts don’t fully click right away. We’ll be exploring them in more detail as we progress. For now, just remember that these are mathematical terms that help us reason about how to combine, apply, and manage contexts in our programs.

The reason we still use these names is to show respect to the mathematicians who developed these ideas. In the FP world, everyone uses these terms and you will learn to use them as well!


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

Notice how the pure function add will always give the same result for the same inputs, while getCurrentTimeAndAdd depends on an external factor, making it impure. 

In Haskell, we must explicitly define impure functions by using the IO Monad. This is part of what makes functional programming so powerful.

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



# Lesson 1: Types and Domain Theory

# Lesson 1 - Types & Domain Theory

## Introduction to Types

### What Are Types?

Types are fundamental to understanding and writing robust code in Haskell. They are not just a way to categorize data but are crucial to how we model the real world within our applications. The closer our types mirror the real world, the easier our applications become to reason about, and the more robust they become. Let’s dive deeper into what types really are:

- **Types as Categorization**: Types categorize data into labelled sets with one or more variations. For instance, a `Bool` type categorizes data as either `True` or `False`, while a `String` type categorizes sequences of characters.
  
- **Types Represent Information**: They represent the kinds of information our application processes. For example, an `Integer` type represents whole numbers, which might be used to count items or represent IDs.

- **Types in Functions**: In Haskell, types are used in functions to ensure that functions receive and return the correct kinds of data. This is crucial because it allows us to predict and enforce the behavior of our functions. Unlike in some other languages, Haskell functions always return a value and cannot mutate state directly. This means that the internal workings of functions must also be correct, as the types will enforce valid input and output.

### Core Type Declarations in Haskell

Before we delve into function examples, let's explore the basic building blocks of Haskell types: data declarations, type declarations, and newtype declarations.

#### 1. **Data Declarations**
   - **Purpose**: A `data` declaration is used to define a new algebraic data type. This is the most common way to define new types in Haskell.
   - **Syntax**: 

     ```haskell
     data Color = Red | Green | Blue
     ```

     Here, `Color` is a new type with three possible values: `Red`, `Green`, and `Blue`. These values are called constructors. The `data` keyword allows us to define a type that can have multiple forms.

#### 2. **Type Declarations**
   - **Purpose**: A `type` declaration creates a type synonym, which is essentially an alias for an existing type. It doesn't create a new type but makes the code more readable.
   - **Syntax**:

     ```haskell
     type Name = String
     ```

     In this example, `Name` is a type synonym for `String`. Everywhere you see `Name` in the code, it is just a `String` underneath, but using `Name` can make your code more descriptive.

#### 3. **Newtype Declarations**
   - **Purpose**: The `newtype` declaration creates a new type that is distinct from its underlying type but has the same runtime representation. It’s often used for type safety without runtime overhead.
   - **Syntax**:

     ```haskell
     newtype CustomerId = CustomerId Int
     ```

     Here, `CustomerId` is a new type that wraps an `Int`. While it’s just an `Int` at runtime, the type system treats `CustomerId` and `Int` as distinct, preventing you from accidentally mixing them up.

#### 4. **Function Definitions**
   - **Purpose**: Functions in Haskell are first-class citizens, meaning they can be passed as arguments, returned from other functions, and assigned to variables. Functions also have types, and these types are crucial in ensuring that functions interact with data correctly.
   - **Syntax**:

     ```haskell
     greet :: Name -> String
     greet name = "Hello, " ++ name ++ "!"
     ```

     In this example, `greet` is a function that takes a `Name` (which is a synonym for `String`) and returns a `String`. The type signature `Name -> String` ensures that `greet` only accepts a `Name` as input and will always produce a `String` as output.

### How Are Types in FP Different?

In Haskell, types are first-class citizens. This means that they are treated as values by the compiler, allowing you to encode complex logic directly into your types. This capability leads to predictable and safe code because the types themselves enforce the rules of your application.

**Comparison with Other Languages:**
- In languages like C#, types are often tied to classes and objects, and while types are important, they don’t offer the same level of expressiveness and safety. In C#, it’s possible to misuse types, especially when dealing with inheritance and mutable state, leading to unpredictable behavior deep in the execution of a program.
- Haskell’s type system, based on the Hindley-Milner type system, is much more powerful. It supports features like type inference, algebraic data types (ADTs), and pattern matching, all of which allow you to write concise yet expressive code without sacrificing safety.

### Why Types Matter

Having strict and well-thought-out types is crucial to robust system design. Let’s break down why:

- **Clarity and Predictability**: Types provide a clear contract for what a function does. When you look at a Haskell type signature, you can often understand what the function does without even seeing the implementation. This clarity is invaluable, especially in large codebases where multiple developers are working together. It reduces the likelihood of bugs and makes the system easier to maintain.
  
- **Avoiding Runtime Errors**: In dynamically-typed or loosely-typed languages, many errors only appear at runtime, leading to potential crashes or unpredictable behavior. Haskell’s strong type system catches these errors at compile time, significantly reducing the risk of runtime failures.
  
- **Reasoning About Code**: In Haskell, you can often reason about what a piece of code does just by looking at its type signature. This allows you to find and apply functions that match the type signature you need, often without needing to write new code. Hoogle, a Haskell-specific search engine, lets you search for functions by their type signatures, making it incredibly easy to integrate the right function into your program.

### Example: Compiler Failures in Procedural Programming

Let’s explore some examples where Haskell’s type system helps prevent runtime errors that are common in procedural languages like C.

#### Procedural Example in C:

```c
#include <stdio.h>

int divide(int a, int b) {
    return a / b;
}

int main() {
    int result = divide(10, 0);
    printf("Result: %d\n", result);
    return 0;
}
```


Expected Error:

- When you run this C code, dividing by zero will lead to undefined behavior, which can cause a crash or other unexpected results.

Typical Error Output:

- Depending on the system, you might see an error like “Floating point exception: division by zero” or the program might just crash without a clear error message.

Safe Haskell Equivalent:

```haskell
divide :: Integer -> Integer -> Maybe Integer
divide _ 0 = Nothing
divide a b = Just (a `div` b)

main :: IO ()
main = case divide 10 0 of
    Nothing -> putStrLn "Cannot divide by zero!"
    Just result -> putStrLn ("Result: " ++ show result)
```

Expected Result:

- This Haskell code safely handles the division by zero case, preventing a runtime error. Instead of crashing, it prints a friendly message: “Cannot divide by zero!”

Explanation:

- The use of Maybe in the type signature indicates that the result of divide might be Nothing (indicating an invalid operation) or Just a value (indicating a valid division). This forces the programmer to handle both cases explicitly, preventing runtime surprises.

#### The Magic of the Hindley-Milner Type System

One of the things that makes Haskell’s type system so powerful is the Hindley-Milner type system, which supports type inference and currying:

- Type Inference: The compiler can often deduce the types of expressions without explicit annotations. This reduces boilerplate and lets you write concise code while still enjoying the safety of static typing.
- Currying: In Haskell, every function is curried by default, meaning that functions with multiple arguments are treated as a series of functions that each take a single argument. This allows for partial application, where a function is applied to some of its arguments, returning another function that takes the remaining arguments. The Hindley-Milner type system ensures that even partially applied functions are type-safe at every step.

#### Example of Currying in Haskell:

```haskell
add :: Integer -> Integer -> Integer
add x y = x + y

increment :: Integer -> Integer
increment = add 1
```

Here, add is a curried function that takes two Integer arguments. By partially applying add with the value 1, we create a new function increment that only takes a single argument and adds 1 to it. The type system ensures that this transformation is valid and safe.

## Simple Types and Type Signatures

### How to Read Type Signatures

Type signatures are an essential part of Haskell programming. They describe the types of the inputs a function takes and the type of output it produces. Understanding how to read and interpret these signatures is critical to writing and maintaining robust Haskell code.

- **Basic Type Signature**: A typical type signature in Haskell looks like this:

  ```haskell
  functionName :: InputType1 -> InputType2 -> OutputType
  ```

This means that the function functionName takes two inputs: one of type InputType1 and one of type InputType2, and it returns an output of type OutputType.

Example:

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

In this example, add takes two integers (Int -> Int) and returns another integer (Int).

- **Complex Type Signatures**: Type signatures can get more complex, especially in larger Haskell libraries like Obelisk or Reflex, where cross-platform mobile development is involved.

Example:

```haskell
widgetHold :: forall t m a. (MonadHold t m, PostBuild t m) => m a -> Event t (m a) -> m (Dynamic t a)
```

Let’s break this down:
- `forall t m a.`: This means the function is polymorphic over types t, m, and a.
- `(MonadHold t m, PostBuild t m) =>`: These are type class constraints, meaning m must be an instance of MonadHold and PostBuild.
- `m a -> Event t (m a) -> m (Dynamic t a)`: This is the core function signature. It takes an m a, an Event t (m a), and returns an m (Dynamic t a).
Even though this looks complicated, it’s fundamentally still the same concept of specifying the flow of types through the function.

- **Partial Application and Currying**: Haskell functions are curried by default. This means every function actually takes one argument and returns another function if more arguments are needed. It’s important to remember that partial application allows you to call a function with fewer arguments than it expects, returning a new function that waits for the remaining arguments.

Currying Example:

```haskell
add :: Int -> Int -> Int
add x y = x + y

increment :: Int -> Int
increment = add 1
```

In this example, add takes two integers, but by partially applying it with the argument 1, we create a new function increment that takes only one integer and adds 1 to it.

Haskell hides this complexity, but understanding currying is crucial when reading type signatures with multiple -> symbols. You can think of Int -> Int -> Int as Int -> (Int -> Int)—a function that returns another function.

Explicit Currying Example:
```haskell
addCurried :: Int -> Int -> Int
addCurried = \x -> \y -> x + y
```

This is equivalent to the earlier add function, but here you can explicitly see how addCurried takes one argument (x), and returns a function that takes the next argument (y), and then produces the sum of x and y.

- A Quick Note on forall: You might come across forall in complex type signatures like the one shown above. forall introduces type variables that can be replaced with any type. It’s often used in polymorphic functions where the types aren’t known in advance. By default, Haskell assumes forall is present in type signatures without it being explicitly written, so you don’t need to worry about it in most cases.

#### Primitive Types

Primitive types in Haskell are the basic building blocks for more complex types. They represent simple values like numbers, characters, booleans, etc. These types are fundamental to every Haskell program and allow you to represent data in the most basic forms.

#### Common Primitive Types

- Int: Represents a fixed-size integer. It’s faster but limited in range. Example: 42.
- Integer: Represents an arbitrary-precision integer, meaning it can handle very large numbers. Example: 12345678901234567890.
- Float: Represents a single-precision floating-point number. Example: 3.14.
- Double: Represents a double-precision floating-point number, which is more precise than Float. Example: 2.71828.
- Char: Represents a single character. Example: 'a'.
- Bool: Represents a boolean value, which can be either True or False. Example: True.
- String: Represents a sequence of characters (which is actually a list of Char values). Example: "Hello, world!".

#### Primitive Type Examples

```haskell
-- Integer
age :: Int
age = 30

-- Floating-point number
piValue :: Double
piValue = 3.14159

-- Boolean
isSunny :: Bool
isSunny = True

-- Character
initial :: Char
initial = 'H'

-- String
greeting :: String
greeting = "Hello, Haskell!"
```

These types are the building blocks for more complex data structures that you’ll define using algebraic data types (ADTs), which we’ll cover shortly.

#### Polymorphic Types

Polymorphic types allow functions to be more flexible by operating on any type. Polymorphic functions are functions that work with type variables, rather than specific types.

**Example of Polymorphic Types:**

```haskell
identity :: a -> a
identity x = x
```

In this example, identity is a polymorphic function. The type variable a can be replaced with any type. This function takes an input of type a and returns a value of the same type.

#### Type Class Constraints

You can restrict polymorphic types using type class constraints. This allows the polymorphic function to work only with types that implement a particular type class.

**Example with Type Class Constraints:**

```haskell
addValues :: Num a => a -> a -> a
addValues x y = x + y
```

Here, `Num a =>` is a type class constraint that restricts a to types that are instances of the Num type class (such as Int, Float, etc.). This ensures that addValues can only be used with numeric types.

### Algebraic Data Types (ADTs)

Algebraic Data Types (ADTs) allow you to define custom types by combining other types. ADTs are foundational to Haskell and functional programming, allowing you to model complex data structures in a way that’s easy to reason about.

#### Sum Types and Product Types

- **Sum Types**: Represent a choice between multiple alternatives. A value of a sum type can take one of several forms.

Example of a Sum Type:

```haskell
data Color = Red | Green | Blue
```

Here, Color is a sum type because a value of type Color can be either Red, Green, or Blue.

- **Product Types**: Combine multiple types into a single type. A value of a product type contains values for each of the constituent types.

Example of a Product Type:

```haskell
data Point = Point Int Int
```

Here, Point is a product type that contains two Int values representing the x and y coordinates of a point.

#### Defining and Using ADTs

You can use ADTs to model complex real-world data in your application.

Example:

```haskell
data Shape = Circle Float
           | Rectangle Float Float
           | Square Float
```

- Shape is an ADT that can be a Circle with a radius (Float), a Rectangle with a width and height (Float Float), or a Square with a side length (Float).

#### Pattern Matching with ADTs

Pattern matching allows you to deconstruct ADT values and apply different logic depending on the constructor used.

Example of Pattern Matching with ADTs:

```haskell
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Square s) = s * s
```

In this example, the area function computes the area of a Shape. It uses pattern matching to determine whether the shape is a Circle, Rectangle, or Square, and calculates the area accordingly.

## Domain-Driven Design

### What is Domain-Driven Design?

Domain-Driven Design (DDD) is a methodology for structuring software around a business domain. It’s all about creating a deep understanding of the business or system you’re building and then reflecting that understanding directly in your code. The goal is to model real-world concepts with code that mirrors the domain as closely as possible.

In essence:
- **Model Real-World Concepts**: In DDD, the focus is on modeling the real world as accurately as possible, avoiding the use of primitive types like `Int` and `String` except at the "leaf" levels (e.g., values or properties that directly map to basic concepts like age or names). Instead, we create types that describe domain-specific concepts like `CustomerId`, `OrderId`, or `RobotArmAngle`. These types are meaningful and self-explanatory in the domain.
  
- **Use Domain Language**: A key principle of DDD is using the same language that the client uses to describe their domain. This means that, when you’re building a system for a specific business or project, you use terms that make sense to the client. Ideally, your client should be able to understand your code—or at least be able to provide meaningful input into the design of your domain models. When you’ve done it right, your code should read like plain English to domain experts.

#### Example of Domain-Driven Design

Imagine you're tasked with building a home robot system, where one of its key functionalities is to interact with objects around the house. The robot needs to perform tasks like picking up objects, turning knobs, and pressing buttons. Your client, an engineer with expertise in robotics, describes the system using terms like “robot arm,” “actuator,” “gripper,” and “torque.”

If you model this system using primitives (`Int`, `String`, `Float`), your code might look something like this:

```haskell
pickUp :: Int -> Float -> String -> Bool
pickUp armId rotation gripper = -- perform task
```

This code uses generic types like Int, Float, and String, which have no meaningful connection to the domain concepts. The client would have a hard time understanding what this function does or whether it models the domain correctly.

Instead, with DDD, you would model the system using types that reflect the domain:

```haskell
data RobotArm = RobotArm ArmId Rotation Torque
data Gripper = Gripper GripperType GripStrength

pickUp :: RobotArm -> Gripper -> Object -> TaskResult
pickUp arm gripper object = -- perform task
```

Now the function pickUp uses domain-specific types (RobotArm, Gripper, Object, TaskResult), making it easier for your client to understand and ensuring that your code accurately models the real-world system. The types tell you a lot more about what’s going on, reducing ambiguity and making your system safer and easier to reason about.

#### Modelling the Domain with Types

Let’s look at a more complex example to illustrate how types can model real-world interactions between different parts of a system.

Example Scenario: Robot Arm and Torso Integration

Imagine a robotics project where one team is responsible for building the robot’s arm, and another team is responsible for the torso. The two components need to be integrated at the shoulder joint. The arm has actuators that rotate at certain angles, and the torso needs to provide power to these actuators. Both teams are working independently, and the shoulder becomes the integration point for the two components.

Hypothetical Background:

- The Arm Team is working with actuators that rotate between 0 and 180 degrees.
- The Torso Team is responsible for delivering power, ensuring that the arm’s actuators receive enough power to function.
- The Shoulder acts as the integration point where the power and rotation capabilities meet.

We need to model the interaction between these systems, ensuring that the power delivered by the torso matches the requirements of the actuators in the arm.

Example Code:


```haskell
-- Define the arm's actuators and rotation
data Rotation = Rotation Int -- value in degrees between 0 and 180
data Actuator = Actuator Rotation Torque

-- Define the torso's power capabilities
data PowerSupply = PowerSupply Torque -- Torque provided to the actuators

-- Define the shoulder as the integration point
data Shoulder = Shoulder Actuator PowerSupply

-- Function to match the power provided by the torso with the actuator's requirements
integrateShoulder :: Shoulder -> Bool
integrateShoulder (Shoulder (Actuator (Rotation angle) torque) (PowerSupply supplyTorque))
    | torque <= supplyTorque = True -- Power matches or exceeds the actuator's requirement
    | otherwise = False -- Insufficient power
```

In this example:

- We’ve modeled Rotation, Actuator, and PowerSupply using Haskell’s type system to reflect the real-world domain.
- The function integrateShoulder checks whether the power supplied by the torso is sufficient for the actuators in the arm to function properly.

This domain model allows us to precisely describe the real-world system, ensuring that the interaction between the arm and torso is modeled correctly. By using meaningful types, we avoid the pitfalls of using primitives like Int and Float, which don’t convey the intent or constraints of the system.

#### Defining Domain Concepts as Types

Haskell’s type system is incredibly powerful and can be used to model much more than just basic data structures. You can define complex concepts using types, allowing you to encode domain-specific logic directly into the type system.

Example: Aggregating Data from Space Agencies

Let’s say we’re building a system that aggregates real-time data from three different space agencies. Each agency provides slightly different data formats, and some of the data might be missing or delayed. To ensure reliability, we need to aggregate the data and serve the most accurate, up-to-date information to the end user. We’ll validate the data using a round-robin algorithm to determine which data is most trustworthy.

Complex Domain Model:

```haskell
-- Define a type for data from Space Agencies
data SpaceAgency = NASA | ESA | JAXA

-- Define a type for the data each agency provides
data SpaceData = SpaceData {
    temperature :: Maybe Float,
    pressure    :: Maybe Float,
    velocity    :: Maybe Float
}

-- A type class to validate data from each agency
class ValidateData a where
    validate :: a -> Bool

instance ValidateData SpaceData where
    validate (SpaceData temp press vel) = all isJust [temp, press, vel]

-- Define a type for the round-robin algorithm to choose the most accurate data
data RoundRobin = RoundRobin SpaceData SpaceData SpaceData

-- Function to aggregate data from multiple agencies and serve the best one
aggregateData :: RoundRobin -> SpaceData
aggregateData (RoundRobin nasa esa jaxa)
    | validate nasa = nasa
    | validate esa  = esa
    | validate jaxa = jaxa
    | otherwise     = error "All data invalid"

-- Example usage
nasaData = SpaceData (Just 20.5) (Just 1013.25) Nothing
esaData = SpaceData (Just 20.4) (Just 1013.30) (Just 7500)
jaxaData = SpaceData Nothing (Just 1013.20) (Just 7501)

main = print $ aggregateData (RoundRobin nasaData esaData jaxaData)
```

In this example:

- We define the SpaceData type, which represents the real-time data from each space agency. Some fields may be missing (Maybe Float), which we handle explicitly.
- The ValidateData type class allows us to check whether the data provided by each agency is complete and valid.
- The RoundRobin type models the round-robin algorithm used to select the most reliable data.
- The aggregateData function aggregates data from NASA, ESA, and JAXA, and returns the most accurate dataset based on the validation criteria.


## Pairing Domain Theory with Type Theory

### Type Theory in the Context of Domain Modeling

In Haskell, type theory goes hand in hand with domain-driven design. Domain theory helps you model the real world by representing domain concepts in code, while type theory ensures that your code is precise, safe, and free from common runtime errors. The power of Haskell’s type system lies in its ability to **perfectly describe your problem space**.

#### How Types Make Domains Safer

When you define a domain model, you're not just giving names to concepts—you're also giving those concepts constraints and rules through types. For example:
- **Simple types like `Int` or `String`** don’t carry much meaning by themselves. They could be anything.
- **Custom types like `CustomerId` or `Temperature`** represent specific concepts in your domain and ensure that values are used in appropriate contexts. By encoding your domain logic directly into types, you prevent mistakes like accidentally passing an email address where a `CustomerId` is expected.

Let’s break this down with a simple example.

#### Example: Defining Safe Domain Concepts

Consider an online shopping platform. You’ll have concepts like customers, orders, and products. You could model these using primitives like `Int` and `String`, but this opens the door to mistakes:

```haskell
-- This is unsafe
createOrder :: Int -> String -> String -> Int -> Bool
createOrder customerId productName address quantity = -- Implementation
```

In this version, we’re using Int and String everywhere, which doesn’t reflect the domain at all. It’s easy to accidentally swap parameters or use invalid values.

With type theory, we can give more meaning and safety to the domain:

```haskell
-- Safe version using custom types
newtype CustomerId = CustomerId Int
newtype ProductName = ProductName String
newtype Address = Address String
newtype Quantity = Quantity Int

createOrder :: CustomerId -> ProductName -> Address -> Quantity -> Bool
createOrder (CustomerId custId) (ProductName prodName) (Address addr) (Quantity qty) = -- Implementation
```

By defining types like CustomerId, ProductName, Address, and Quantity, we give more meaning to our code. The type signature for createOrder now clearly reflects what the function does, and we prevent mistakes like passing an address where a ProductName is expected.

### Ensuring Valid States with Types

One of the most powerful aspects of Haskell’s type system is that it allows you to enforce valid states directly in the type system. This means you can encode business rules or constraints into your types, making it impossible to represent invalid states.

#### Example: Preventing Invalid Orders

Let’s extend our Order example. In a typical e-commerce application, an order should never have a negative quantity, and it should always have a valid product. We can enforce these rules in the type system.

Basic Example (with risk of invalid states):

```haskell
data Order = Order Int String Int -- CustomerId, ProductName, Quantity
```

In this version, there’s nothing stopping us from creating an Order with an invalid product name or a negative quantity.

Improved Example (enforcing valid states):

```haskell
newtype CustomerId = CustomerId Int
newtype ProductName = ProductName String
newtype Address = Address String
newtype Quantity = Quantity Int

data ValidatedOrder = ValidatedOrder CustomerId ProductName Address Quantity
```

But we can go even further to ensure that only valid orders can exist:

```haskell
newtype NonEmptyString = NonEmptyString String
    deriving (Show)

mkNonEmptyString :: String -> Maybe NonEmptyString
mkNonEmptyString "" = Nothing
mkNonEmptyString str = Just (NonEmptyString str)

newtype PositiveInt = PositiveInt Int
    deriving (Show)

mkPositiveInt :: Int -> Maybe PositiveInt
mkPositiveInt n
    | n > 0     = Just (PositiveInt n)
    | otherwise = Nothing

data Order = Order {
    customerId :: CustomerId,
    productName :: NonEmptyString,
    address :: NonEmptyString,
    quantity :: PositiveInt
}

createOrder :: CustomerId -> String -> String -> Int -> Maybe Order
createOrder cid pName addr qty = do
    pName' <- mkNonEmptyString pName
    addr'  <- mkNonEmptyString addr
    qty'   <- mkPositiveInt qty
    return $ Order cid pName' addr' qty'
```


In this version:

- NonEmptyString ensures that product names and addresses can never be empty.
- PositiveInt ensures that quantities are always positive.

The createOrder function now returns a Maybe Order, meaning that it will only produce a valid Order if all the inputs meet the necessary conditions. This way, you can guarantee at compile time that no invalid orders can be created.

### How the Compiler Infers Types

Haskell’s compiler is powerful enough to infer types even if you don’t explicitly annotate them. However, it’s always a good idea to include type signatures for clarity and better error checking.

### Example of Type Inference:

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

Even if you omit the type signature, Haskell can infer that add takes two Ints and returns an Int:

```haskell
add x y = x + y -- Haskell infers: add :: Int -> Int -> Int
```

### Type Inference with Polymorphism:

Haskell can also infer types for polymorphic functions. For example:

```haskell
identity x = x
```

Haskell infers that identity can work with any type, so the type signature becomes:

```haskell
identity :: a -> a
```

In this case, a is a type variable, meaning that identity can take and return any type.

#### Practical Example: Modelling a Bank Account System

To see how type theory and domain modeling work together in a real-world scenario, let’s model a banking system.

1. Domain Concepts:
- Account: Represents a bank account.
- Transaction: Represents a deposit or withdrawal.
- Balance: Represents the current balance of the account.
2. Domain-Specific Types:
- We’ll define custom types for these concepts to ensure that invalid states (e.g., negative balances, invalid transactions) are impossible.

Example Code:

```haskell
newtype AccountId = AccountId Int
newtype Amount = Amount Float
    deriving (Show)

data TransactionType = Deposit | Withdrawal
    deriving (Show)

data Transaction = Transaction {
    transactionType :: TransactionType,
    amount :: Amount
} deriving (Show)

data Account = Account {
    accountId :: AccountId,
    balance   :: Amount
} deriving (Show)

-- Function to apply a transaction to an account
applyTransaction :: Account -> Transaction -> Maybe Account
applyTransaction (Account accId (Amount bal)) (Transaction Deposit (Amount amt)) =
    Just (Account accId (Amount (bal + amt)))

applyTransaction (Account accId (Amount bal)) (Transaction Withdrawal (Amount amt))
    | bal >= amt = Just (Account accId (Amount (bal - amt)))
    | otherwise  = Nothing -- Prevent overdraft

-- Example usage
main = do
    let account = Account (AccountId 1) (Amount 1000)
    let deposit = Transaction Deposit (Amount 200)
    let withdrawal = Transaction Withdrawal (Amount 1500)

    print $ applyTransaction account deposit      -- Valid deposit
    print $ applyTransaction account withdrawal   -- Invalid withdrawal (overdraft)
```

In this example:

- AccountId, Amount, and Transaction are modeled using custom types.
- The applyTransaction function ensures that withdrawals can only happen if there’s enough balance, preventing overdrafts.
- This ensures that invalid states (e.g., negative balances) are impossible to represent.


## Advanced Type Concepts

In this section, we’ll introduce some of Haskell’s more advanced type concepts, such as type classes, type families, phantom types, and type-level programming. You don’t need to learn these concepts right away, but knowing how to recognize them and having a basic understanding of how they work will help you expand your knowledge over time.

### Type Classes and Overloading

**Type classes** are one of the most powerful features in Haskell. They provide a way to define generic interfaces that can be implemented by different types. Type classes allow for **ad hoc polymorphism**, meaning that you can write functions that work with any type as long as that type implements certain behavior (i.e., is an instance of a type class).

Think of type classes like interfaces in object-oriented programming. However, type classes are more flexible and allow Haskell’s type system to express a wide range of concepts while ensuring type safety.

#### Example: The `Eq` Type Class

The `Eq` type class defines an interface for equality testing. If a type is an instance of `Eq`, you can use the `(==)` and `(/=)` operators to compare values of that type.

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

-- Making a custom data type an instance of Eq
data Color = Red | Green | Blue

instance Eq Color where
    Red == Red     = True
    Green == Green = True
    Blue == Blue   = True
    _ == _         = False

-- Example usage:
isEqual :: Color -> Color -> Bool
isEqual Red Green = Red == Green -- False
```

In this example:

- We define the Color data type with three possible values: Red, Green, and Blue.
- We make Color an instance of the Eq type class by defining how equality works for the Color type.
- Once we’ve defined this, we can compare Color values using the == operator.

### Overloading Functions with Type Classes

Type classes also allow us to overload functions. A function can behave differently depending on the type of the arguments it receives, as long as the type is an instance of the relevant type class.

For example, the + operator is overloaded by the Num type class, which defines numeric operations:

```haskell
class Num a where
    (+) :: a -> a -> a
    -- other numeric operations...
```

This means that + can be used with any type that’s an instance of Num (like Int, Float, etc.).

### Polymorphism with Type Classes

You can define polymorphic functions that work with any type that’s an instance of a specific type class. Here’s an example of a polymorphic function that works with any type that implements the Show type class (which is responsible for converting values to strings):

```haskell
printValue :: Show a => a -> String
printValue value = "The value is: " ++ show value
```

Here, Show a => is a type class constraint. It means that printValue can accept any type a as long as a is an instance of the Show type class. The function then uses the show function (which converts a value to a String) to display the value.

### Type Families

Type families are a more advanced feature that allows you to associate types with type classes, effectively creating a form of type-level functions. They enable more flexibility and power in type classes, allowing for more dynamic behavior based on types.

#### Example: Type Families for Different Container Types

Let’s say we want to define a type class for container-like data structures (e.g., List, Maybe, etc.), but we want to allow different types of elements in these containers. Type families can help us here.

```haskell
{-# LANGUAGE TypeFamilies #-}

class Container c where
    type Element c
    empty :: c
    insert :: Element c -> c -> c

-- List instance of Container
instance Container [a] where
    type Element [a] = a
    empty = []
    insert x xs = x : xs

-- Maybe instance of Container
instance Container (Maybe a) where
    type Element (Maybe a) = a
    empty = Nothing
    insert x _ = Just x
```

In this example:

- We define a Container type class with a type family Element c, which represents the type of elements that can be stored in the container.
- We then provide two instances: one for lists ([a]) and one for Maybe a. Each instance defines what kind of elements can be inserted and how to handle insertion and the “empty” state.

### Phantom Types for Extra Type Safety

Phantom types are an advanced feature in Haskell where a type parameter is included in the type definition but is not used in the actual data. They are useful for adding extra type safety to your programs, especially when you want to encode additional constraints or metadata into the types.

#### Example: Phantom Types for Unit Safety

Let’s say we want to create a system that tracks distances, but we want to make sure that we never accidentally mix up meters and kilometers.

```haskell
{-# LANGUAGE GADTs #-}

data Meters
data Kilometers

data Distance a where
    MkDistance :: Double -> Distance a

convertToKilometers :: Distance Meters -> Distance Kilometers
convertToKilometers (MkDistance d) = MkDistance (d / 1000)

-- Example usage
distanceInMeters :: Distance Meters
distanceInMeters = MkDistance 5000

distanceInKilometers :: Distance Kilometers
distanceInKilometers = convertToKilometers distanceInMeters
```

In this example:

- We define two phantom types, Meters and Kilometers, which represent the units of distance.
- The Distance a type takes a phantom type a, which is either Meters or Kilometers. This ensures that distances are always associated with a specific unit.
- We define a convertToKilometers function that safely converts a distance in meters to kilometers.

With phantom types, you can ensure that your code is type-safe at compile time, preventing errors like mixing up units of measurement.

### Type-Level Programming

Haskell allows for type-level programming, where types can be manipulated much like values. This opens the door to highly expressive and type-safe code. You can perform computations and enforce constraints at the type level, reducing the need for runtime checks.

#### Example: Type-Level Natural Numbers

We can use type-level programming to represent physical dimensions (e.g., length, mass, time) and ensure that our units are consistent in mathematical operations.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

data Meter
data Second

data Quantity (unit :: *) where
    MkQuantity :: Double -> Quantity unit

-- Adding two quantities of the same unit
addQuantities :: Quantity unit -> Quantity unit -> Quantity unit
addQuantities (MkQuantity x) (MkQuantity y) = MkQuantity (x + y)

-- Example usage
distance :: Quantity Meter
distance = MkQuantity 100

time :: Quantity Second
time = MkQuantity 9.58

-- This works:
sumDistance = addQuantities distance distance

-- This would fail to compile if uncommented (mismatched units):
-- sumTimeDistance = addQuantities distance time
```

In this example:

- We define a Quantity type that takes a unit (e.g., Meter or Second) as a type parameter.
- The addQuantities function ensures that we can only add quantities of the same unit.
- If we try to add a distance to a time, Haskell’s type system will catch this error at compile time.

## Recap & Exercises

### Recap

In this lesson, we’ve explored the foundational concepts of types and domain theory in Haskell. By understanding how types work in Haskell and how to apply domain-driven design, you’re now equipped with the tools to model real-world domains accurately and safely. Let’s review the key takeaways from this lesson:

- **Simple Types and Type Signatures**:
    - Type signatures describe the inputs and outputs of functions, providing a clear contract for what a function does.
    - Primitive types like `Int`, `Float`, `Bool`, and `String` are the basic building blocks for more complex data structures.
    - Haskell’s type system is based on currying, meaning that every function takes one argument and returns a new function if more arguments are required.
    - Polymorphic types allow functions to work with any type, providing flexibility and reuse.

- **Domain-Driven Design**:
    - Domain-driven design (DDD) is a methodology for structuring software around real-world business domains, using types to model the domain in code.
    - Avoid using primitive types like `Int` or `String` for domain concepts. Instead, define domain-specific types like `CustomerId`, `OrderId`, or `ProductName` to make your code more meaningful and safe.
    - Use the language of the business domain when modeling in code, ensuring that the types and structures reflect real-world concepts accurately.

- **Pairing Domain Theory with Type Theory**:
    - Haskell’s type system allows you to enforce valid states in your domain model by encoding business rules directly into the type system.
    - Type safety prevents invalid states, such as negative quantities or empty product names, from existing in your system, reducing runtime errors.
    - The Haskell compiler works with you to infer types, but explicit type annotations provide clarity and enforce constraints.

- **Advanced Type Concepts**:
    - **Type Classes**: Haskell’s type classes allow for ad hoc polymorphism, providing a way to define generic interfaces that can be implemented by different types.
    - **Type Families**: Type families let you associate types with type classes, allowing for more flexible and dynamic behavior based on types.
    - **Phantom Types**: Phantom types allow you to add extra type safety without affecting runtime behavior. They’re especially useful for encoding additional constraints or metadata into the type system.
    - **Type-Level Programming**: Haskell allows for type-level programming, enabling you to enforce constraints at the type level and reducing the need for runtime checks.

These are the building blocks of functional programming in Haskell and the key to creating robust, type-safe applications. By mastering types and domain modeling, you’re well on your way to becoming a proficient Haskell developer.

### Exercises

Now it’s time to put what you’ve learned into practice. The following exercises will help reinforce the concepts from this lesson by challenging you to think through real-world domain problems and apply Haskell’s type system effectively.

#### Exercise 1: Modeling a Banking System

**Objective**: Create a simple banking system using domain-driven design principles.

- Define the following types:
    - `AccountId`: A unique identifier for a bank account.
    - `Balance`: The current balance in the account (use a custom type to prevent negative balances).
    - `TransactionType`: An algebraic data type representing deposits and withdrawals.
    - `Transaction`: A record type representing a transaction (including the type and the amount).
- Write a function `applyTransaction :: Balance -> Transaction -> Maybe Balance` that updates the balance based on the transaction. Ensure that withdrawals cannot exceed the balance.

**Hint**: Use a custom type to ensure that `Balance` can never be negative.

```haskell
-- Example skeleton
newtype Balance = Balance Float
data TransactionType = Deposit | Withdrawal
data Transaction = Transaction { tType :: TransactionType, amount :: Float }

applyTransaction :: Balance -> Transaction -> Maybe Balance
-- Implement your function here
```

#### Exercise 2: Aggregating Data from Multiple Vendors

**Objective:** Model a system that aggregates data from three different vendors.

- Define a custom type Vendor with constructors VendorA, VendorB, and VendorC.
- Create a type DataFeed to represent real-time data from each vendor (e.g., temperature, humidity, and pressure).
- Write a function aggregateFeeds :: DataFeed -> DataFeed -> DataFeed -> DataFeed that combines the data from each vendor, selecting the most recent valid data for each field.

**Hint:** Use Maybe types for fields that might be missing, and use pattern matching to choose valid data.

```haskell
-- Example skeleton
data Vendor = VendorA | VendorB | VendorC
data DataFeed = DataFeed { temperature :: Maybe Float, humidity :: Maybe Float, pressure :: Maybe Float }

aggregateFeeds :: DataFeed -> DataFeed -> DataFeed -> DataFeed
-- Implement your function here
```

#### Exercise 3: Using Phantom Types for Unit Safety

**Objective:** Prevent unit mix-ups using phantom types.

- Define two phantom types Meters and Kilometers.
- Create a Distance type that uses phantom types to represent distances in meters or kilometers.
- Write a function convertToKilometers :: Distance Meters -> Distance Kilometers that converts a distance in meters to kilometers.
- Try to add distances in meters and kilometers together, and make sure the compiler prevents it!

```haskell
-- Example skeleton
data Meters
data Kilometers

data Distance a = MkDistance Double

convertToKilometers :: Distance Meters -> Distance Kilometers
-- Implement your function here
```




**Lesson 2 - Immutability Changes Everything**

Currying, pure evaluation forced, benefits of this

if you think wbout it, mutability doesnt a actually exist in the real world. x += 5 holds no realworld meaning.

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

**Lesson 7 - Recursion**
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

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

**Understand the task**: We need to create an order record that includes details like the items purchased, the total cost, and the status of the payment.
**Missing knowledge**: What information needs to be included in the order? How should we handle failed payments?
**Write the function**:

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

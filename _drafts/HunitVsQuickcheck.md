---
layout: post
title:  "HUnit vs. QuickCheck"
author: Leonhard Applis
description: A thought on unit-testing and property-based-testing
categories: [Testing, Haskell]
---

I love testing. I really do.
Especially on the unit- and integration-level.

Seeing tests go green fills my heart with joy, and thinking up tests feels for me like a kid who build a lego-castle and now plays with figures in it.

When I started my journey of Haskell, *Real World Haskell* introduced me to QuickCheck, which is a framework for property-based testing.

Instead of a normal test-setup and your common *arrange-act-assert* pattern you follow f.e. in Java,
you describe a property which should hold true for the unit to test,
and in addition an *arbitrary*-instance of your test-object, that is a way to generate a random test-object.
QuickCheck will run over all your properties, insert random generated test-objects, and if the property holds for a hundred random samples the test passes.

I was stunned. The examples given from QuickCheck and *Real World Haskell* are very verbose and precise, while also incredibly powerful.
In addition, randomized testing is known to reach better coverage, as sooner or later every unit-test one can manually write will be in the random samples.
So my path was set: QuickCheck is the better tool, the mightier approach and from now on it is only QuickCheck.

It was a brave new world, and I set out to fail.

Don't get me wrong - I still like QuickCheck.

However its just a tool and I want to present some cases I came across where I'd choose the tool and where I'd avoid it.

The following examples come fresh from my pet-projects, and while I'm for sure no grand-master and there are ways to do it better,
they are maybe problems everyone can encounter in a similar way.

## QuickChecks' Pitfall 1: Expressiveness

I was writing a simple calculator, which parses a given formula from a string and calculates it if possible.
It can do some unary and ternary operators, and variables.

Some of the most important use-cases included:

- Checking that brackets are applied properly, so that (1+2)*3 = 9
- Checking that brackets *stack* properly, so that ((1+2)*(1+2))+(2*2) = 13
- Checking some error cases, such as *2*log* --> "missing argument for log"

These are fairly easy to write as unit tests, and the unit-tests themselves are verbose.
An error in one of the test-cases is easily visible and verifiable.

It is possible to create arbitrary instances for these cases.
Overall, a faulty log should always yield an error. That is a valid property, and one we are interested in.

How would this property look like?

```Haskell
parse :: Text -> Either Text SyntaxTree

prop_faultyLog_parseError faultyLogFormula =
    case parse faultyLogFormula of
        Left msg -> msg == "missing argument for log"
        Right _ -> False
```

That's an easy property, not much to see here.
But it leaves one question: How would you make the random faulty-string?

There are ways to do it, such as adding it to the end of a valid formula.
This however expect you to be able to generate valid formulas.
In general, I think there is not much grace in spending time to auto-generate failures, and there are infinite possibilities to do so.

The real problem boils down to this:

A test should test logic, and not introduce logic.
While the property may be simple, the *arbitrary* instance is not - it is a potentially huge and debatable implementation.

Think about the bracket-example:

Writing an arbitrary instance for this can be somewhat easy be done by defining the depth, an arbitrary instance for an un-bracketed formula and some randomness.
I think everyone will be able to do that. However, it's not:

- easy to keep track of the result of that formula
- easy understandable
- necessary to write a successful, meaningful test

compare all this hassle and potential fight with your colleagues with the following:

```Haskell
testParse_faultyLog_shouldResultInMissingArgumentMessage =
    let formula = " 2 * 3 + log"
    in "missing argument for log" ~=? getLeft (parse formula)
```

It's simple, readable, and gives you value.
It doesn't slow you down and you have no additional code to justify to anyone.

## QuickChecks' Pitfall 2: Complexity

Another pet-project I had was a small chess-game for the console.
As an additional quirk I didn't store the board as a matrix, but instead as a list of figures.

Some of the most difficult tests included:

- checking that a figure can move *onto* enemy figures and remove them from the board
- a pawn can be exchanged for **any** missing figure
- checking that a figure can't move onto friendly figures

The tests usually first build a valid board, sometimes deriving from factory methods.
Then I checked for all potential moves and the resulting boards,
and looked whether the amount of potential moves was right and that some edge-cases are either true or false.

This approach is easy but rather noisy. It has at least 5 lines of code for every test-case, which is quite a lot for Haskell.
The tests are also not well understandable - as either a lot is happening in factory methods, or the test itself is very cluttered.
This made me intensively think about property-based testing and where to go with this.

Defining the properties would be fairly easy again - properties such as `prop_canMoveOntoEnemyFigures` or `prop_pawnCanBeReplaced`.

But the true problem is the following:

**Randomly generating valid Chess-Boards with certain attributes.**

Where attributes can be for example that a pawn is about to reach the other side of the board or your king is in chess.

The only fairly easy way to generate those I can think of is to start from a normal board and take random moves until the conditions are met.
However this assumes you are able to make valid moves and can verify that the conditions are met - which themselves need tests.

I am sure there are very sophisticated approaches solving this, or simpler ones such reading them from a chess-database.

There are three problems remaining:

1. The algorithmic approach can be more complex than your actual project
2. The resource-driven approach introduces new dependencies
3. Sometimes, you are making something truly new - and you cannot compare your results with something existing.

Especially the last one was scary to me - because in the unknown you want to have your tests.

## QuickChecks grace - in properties we trust

One apprentice I supervised was very happy with puzzle-solving programming and any kind of complex task you can give him.

It was true bliss to see him solve the first concept of composition and functors or his own linked list.

We covered the topic of tests and to prove him the value I provided him with a handful of properties for AVL-trees.
The arbitrary instance was simply putting a random list of integers into the trees `fromList` (which was undefined yet).

I remember how I struggled implementing AVL-trees and always missing out one requirement or the other.
Back in my first year of university I would have never come up with the idea of writing a test-suite to speed up things.

The stable and rich test-suite enabled him to solve it in about two days.
Not only he got it *working* - but he actually understood those trees.
We lived happily ever after.

This is a bonus-point for learning with test suites - but that's true for QuickCheck and HUnit alike.

The bonus of QuickCheck is the expressiveness in this case.
As this time there was no issue with defining arbitrary, and the properties are basically given,
providing him with this test-suite took less than 30 minutes.
Altering the test-suite from integers to strings or custom data-types would be no problem.

QuickCheck has been simply the better tool for this job.

## HUnits grace - regression instead of regrets

We know this - we are up to 90% coverage - the sky is blue and the birds are singing along with you and your functions.
Once you want to showcase your idea, all your effort, the very first person has one idea and exactly this idea breaks the code.

You crawl back to your GHC and lick your wounds. You just got a bug.

If you are unlucky enough to have this at work, you now got a Jira-Ticket and have to do a proper regression test in addition to the fix.

With HUnit you can simply reproduce the input into your function, and compare the output.
It may take long to fix your code, and you may decide to redo parts and you have to adjust verbose HUnit-tests.

What are your options?
You can try and express your bug as a property. This often will leave you in a situation like *Pitfall 1* - its hard to express misbehaving as a property.

Ok, you pull it off:
Your write an arbitrary instance, you write the property, you fix the code.

You are showing it to the person who found the bug. You know have to defend your fixes, your arbitrary instance and maybe your property.

If you'd have just had a unit-test you can tell him "this is what we put in last time - it's working now". Fair and Square.

I'd always do regression tests and bug-tests with classic-unit-tests. First of all because I can usually easily write them, so I don't postpone them, and second of all because they are more expressive towards someone who doesn't want to know about properties and arbitraries.

Maybe your bugs show a missing property you have not covered. good!
Add the Regressiontest in HUnit and the property in QuickCheck.
But keep your verbose Regressiontest in case someone wants to check on your code.
Because *proofing* that your property eliminates the regression test might not be that easy as just keeping the duplicate.

## Conclusion

While my joy in testing and my curiosity in QuickCheck are still high,
after these pitfalls I noticed that classic unit-tests still have their place in this world.

The breaking point of using QuickCheck is nearly always, whether you can easily define an arbitrary instance.
And easy usually does not mean you alone - but also that it makes sense for others.

For my latest, little bigger project I therefore used a hybrid approach - the more atomic building bricks are tested with QuickCheck,
while the complex and often user-facing functions are Unit-Tests.

This feels healthy, looks fine and takes the best approach of both worlds.
The user-facing tests can be seen as documentation, while the internal parts are very safe.
Regression-tests in HUnit where easy to write and are more verbose when describing errors and failures than making anti-properties.
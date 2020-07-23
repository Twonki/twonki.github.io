---
layout: post
title:  "Holding a Unit-Test Workshop - Part I"
date:   2020-06-25
author: Leonhard Applis
description: Lessons learned from two workshops hold
categories: [Testing, Java, Teaching]
reading: 10
---

A friend of mine wanted to write his bachelor-thesis on comparing (modern) learning types in the context of a workshop. 
In particular, he wanted to inspect micro-learning and game-based learning for their fitness on introduction-level topics in IT/Programming.
As looking up theory, organizing the workshop and creating the content was a bit too much, he asked me for help and I offered to hold it on the topic of unit-testing in Java.
With Java being part of the curriculum of the trainees and the corporate motto was "Testing is part of our live" we got green light and started preparations.

So, with this blog post I want to show the concepts and the ideas we had, as well as a short overview of their technical and conceptual issues.
The total results - so which group resulted better and where to go next, will be part of a second post. 
This post should be worth reading if you are holding small workshops too and are looking for inspiration on this topic.

The presentation and both workshops are [publicly available and open source](https://github.com/Twonki/UnitShop).

## Content of the Workshop

The topic covered was "Unit-Testing in Java" and consisted of 4 topics:

1. Basics (testing a single class / method)
2. Using Stubs 
3. Using Mocks
4. Best Practices & Test Driven Development

Where the first 3 were accompanied with exercises and the best practices (such as naming conventions) were shown passively in the tasks.
For the sake of fun and the idea of progress, we gave the workshop a theme.
In the first task, the attendees had to test a class *Food*, in the second task an *Ant* (which was tested using a food-stub) and in the third task an *Hive* (which required an ant-mock for testing).
When everything was running properly, a simple game could be run where the ant-hill would sent out ants to eat food and make new ants. 
Code-Wise, except for the code the attendees had to write, both workshops used the same interfaces and where provided roughly the same input.

Both workshops were held with ~10 voluntary attendees in their first or second year of studies (mostly first).
It took four hours and a lunch break.
At the end of the workshop, the participants had to write a small test (2 pages, also in the repository), but the results will be shown in a second part.  

## General Technical Issues

Before we dive into the specific workshops, I want to cover some general issues and propose solutions in hind-sight. 
We did not try the solutions after we found the issue in the first workshop, so that both groups had the same hardships (I know, that's not perfect for the attendees but required for comparable results).

So the first group of issues was a magnitude of problems caused by *bring your own device*. 
And I now wholeheartedly understand why you prefer ancient computers in lectures and workshops over everyone bringing along their device. 
From many disappointing problems of various origins, this is easily amongst the top 5 alongside CSS-Auto-Layout and Java integer division.
The attendees were beforehand told to install Java,Git and Eclipse with a reasonable detailed instruction (no screenshots, but all steps and links provided).
Guess what - of course some didn't. 
Apart from this obvious problem, there were some issues arising with people who had Java already installed and had to re-set their Java-Home to the new version, or in a similar way mixed up eclipse-configurations.
Another issue was some people installed everything in english, while others installed it in german (I am from germany btw), making general instructions and "click here do that" problematic. 
All this happened on the same corporate devices - so at least everyone was on the same operating system and hardware. 

How would I address this in hind-sight? 
The first approach would be to use a virtual machine with a preinstalled image or a docker-container. 
However, this would only shift the issue of installing three things for installing one big-thing. It might be better than the initial approach but does not address everything.
I think the best solution is to use a cloud- or web-based solution.
The strongest option is a full cloud-IDE such as [Gitpod](https://www.gitpod.io/) or its unbranded sister [Eclipse Che](https://www.eclipse.org/che/).
Those IDEs require an image-file with dependencies similar to a docker-file, along a definition of their primary instructions (for example: "Which action to do when pressing 'run'" - in a maven project that is mvn run).
From that position every user can spawn their own perfectly functional workspace that looks like a normal Maven Project. 
The downside of this is mostly the overhead for the team who has to provide both a server running Che+Users and the image files. I consider this the best solution from the participants point of view. 
The second solution to his is a web-based IDE with [Jupyter](https://jupyter.org/). Despite Jupyters popularity with Python, the Jupyter Server supports other languages as well, such as Java.
Jupyter would offer to enrich the program files with images and markdown, which might result in a better overall experience, 
but in terms of Java you have weird things such as declaring classes after each other. 
Therefore using Jupyter would make the tasks "less normal" as you are simply missing normal structure.
But in my opinion Jupyter is considerable, especially if cloud IDEs look scary to you or are in respect of the workshop size not worth the effort.

Other issues rose with Java (which most of the students had in lectures) and Git (which was new to most students). 
But just because you had Java lectures, doesn't mean that you understand the basic principles and *where to put stuff*. 
So there was a visible gap between attendees who used Java or C# in a project and those who didn't. 
Git made less problems than java, but only because the use of git was very similar for every attendee and the users could easily help each other. 

Addressing these issues is rather hard. 
Putting up CV-requirements for the workshop is likely un-helpful with gaining interest and attendees.
One way to solve this is to give a 1-hours refresher beforehand of the actual workshop, but this is time consuming and might bore people familiar with it. 
Another way, which I consider better, is to hand out "cheat-sheets" with the most important terms and ideas. 
This will not help with actually teaching someone how to java - that would be a huge sheet. But it helps people to ask precise questions. 

## The Microlearning Workshop

[Microlearning](https://en.wikipedia.org/wiki/Microlearning) refers to a method of learning where one goes *small steps*, 
with doing small exercises frequently and subsequently. 
The idea of microlearning is particular common in learning languages (human languages, not programming languages), where you repeat 20 words per day. 
The Antipattern to microlearning is the classic math-exercise where you get a setup and the final question, but no instructions for intermediate steps.
As a rule of thump, microlearning exercises should take atmost a few minutes. 

For the workshop we defined a single exercise as either implementing a function under test, or a single unit test.
We split the exercises into *three rounds*, each after their corresponding part in the presentation. 
For the first round they had to implement a given set of tests, where the name of the test was given. 
For the second round, they had to implement tests using stubs and write some of the method bodies in a *test-driven* way. 
In the last round, for mocks, they were given a mix of tests and method bodies in various steps of implementation.

Unlike the Game based approach there was less *controlling* what people did. 
We just looked over their shoulders, or helped people who reached out to us. 
This was quite chill and everyone could work in their pace. The time given for the exercises was enough in general. 

## Microlearning Take-Aways

So the first take-away of microlearning is that people show up with different knowledge and they can solve the tasks in really different times. 
Some people know java and do a fail-fast approach, hitting compile after every line, 
others lacked Java but had the whole ideas figured out and just asked us for help in implementation.
In general just one or two participants "did perfect" but everyone seemed to learn one thing or the other - not necessarily unit testing. 

I am not sure how to address this - after all it's not too bad but on the other side the workshop was about learning testing.
I think one way is to get a more consistent group of attendees.

Another thing was that people finished early and helped others. 
That is of course very friendly, but it might be better if they get more work for themselves. 
But if you put up a suite of 100 tests for a single exercise, it looks frightening for the slower attendees.
An idea would be to have modules like *basics* and addons such as *error-testing* or *parameterized-tests*. But this has many features of game-based learning as this is very similar to side-quests.

## The Game-Based Workshop

In [game-based learning](https://en.wikipedia.org/wiki/Educational_game) the goal is to ease learning with adding common game elements. 
These can include achievements, competition or *story*. The terms "educational game" and "gamification" are sometimes mixed up and a bit blurry, so, in general I'd say that gamification is adding game elements to a non-game structure, while an educational game adds educational elements to a game. They are starting from opposite sides of the same spectrum.
I think we played an educational game with our students, which went as follows: 

We split the participants into two teams of roughly the same size. 
The teams were given a task to implement unit-tests against and (yet) empty class or interface based on the requirements given in the task.
Once the time was over, the teams swapped their code, and now Team A had to fulfill the tests of Team B and Team B had to fulfill the tests of Team A.
The coding-task was either solved once every test is green or when the time was over. 
If the time-out ended the round, the team with the higher percentage of green tests won.

In addition, we put up a sheet of [rules](https://github.com/Twonki/UnitShop/blob/master/Game/Rules.md) which had the game-rules as well as a definition of an foul written in stone. 
Before the teams swapped their code, the judges looked over the tests to spot fouls. 

The swapping was done using git branches, so Team A pushed their tests to a branch 'TeamA_Task1_TestsReady' etc. 

## Game-based Takeaways

The first thing that has to be said: It was pretty competitive. 
Despite the winner not actually gaining anything, everyone took it pretty serious and wanted to win. 
I think this is not an issue, but an important take-away. It is enough to have points and have the game in a mode that you can win even in the last round.

Also the teams developed tactics. 
Writing many (trashy) tests gave the enemy team a good opportunity for easy points, so everyone focussed on writing hard tests and tests for edge-cases. 
Others tried to test every method, so the other team has to definitely implement it.
Unlike the differences in attendees-skills mentioned in microlearning, the strengths of the teams were quite additive. 
If one had figured out how to use stubs properly, and someone else was not having issues with Java, the team was performing generally well.
This kind of team-building was impressive given their setup time.

Second thing is, that it was sometimes a bit unordered. 
Teams tried to distract each other from the task in various ways, both with verbal interaction but also with un-necessary code. 
There were no fouls whatsoever, but the variable names I tell you were horrifying. 
Some insults in the comments etc. 
We caught most of the shenanigans with the definitions of fouls and the requirement that the tests are run-able. 

I think this can be solved with splitting the teams in different rooms. 
But to be honest it gave it a nice atmosphere and everyone had fun.
Maybe with splitting the groups physically it would be too competitive in the end, and harder to help (and control) the teams. 

The judges were mostly not necessary. 
The only fouls we found and rejected were tests that were not clearly matched upon the requirements (such as, something was testing for a default value while it should throw an error).
But I think without judges people would do more fouls.

In hindsight, I would limit the teams to 3 people with rotating *A tests B, B tests C, C tests A* in case of an uneven number of teams and a tournament mode for an even number of teams.
We've seen that some people had little chance to participate in a 5 member team, simply as only one can write at a time. 
Also I would average the percentage of tests per round, to make come-backs easier. Otherwise maybe a team looses the first 2 rounds and looses motivation for round 3.

## Lessons Learned

So the first lesson learned: It was fun! 
Everyone was pretty engaged and the participants enjoyed seeing someone put effort into a workshop.

Except for the things mentioned in their regarding section, I think all of this needs more time than 4h. 
This is a bit inconvenient, but in general I'd do a full-day workshop for both now.
Then you can deal with all the setup issues, java issues and get everything done properly. 

If we were not to do a comparison, I'd change the workshop to be a mix of both approaches. 
Maybe start with a round of microlearning, so everyone is really set up, and contribute his points towards the teams.
Then once everyone understood the basics, start the real team-game as in the game-based approach.

I think with another iteration and the lessons learned, this is a good workshop with a one-time setup effort. 
In the next post (or maybe the one after), I'll address the statistical results of both workshops tests, which had surprising results.
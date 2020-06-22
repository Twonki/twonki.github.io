---
layout: post
title:  "Holding a Unit-Test Workshop - Part I"
date:   2020-06-22
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

## Microlearning Take-Aways

- Really different paces
- 

## The Game-Based Workshop

- The fragments created by the students were put into a seperate branch 

## Game-based Takeaways

- Unordered
- Competetive
- Teams were build
- Judge Role was not (seriously needed)

## Lessons Learned

- More Time 
- Everyone had fun 
- This is a good idea in general

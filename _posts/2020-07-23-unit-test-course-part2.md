---
layout: post
title:  "Holding a Unit-Test Workshop - Part II"
date:   2020-07-23
author: Leonhard Applis
description: Comparison of Microlearning and Gamebased Learning
categories: [Testing, Java, Teaching]
reading: 5
---

After we looked at [the unit-test workshop](https://github.com/Twonki/UnitShop) and discussed some lessons learned, what to do different, etc., 
this post will cover the actual results. 
To me, they were a bit different than what I had expected. 

## Data Collection 

In the end of the workshop, students had to take a test and fill out knowledge-questions in various (easy) formats. 
There were no long calculations and free-text fields or something alike, rather asking for key-words or linking related tasks. 

The students could score 25 Points in knowledge in total. 
After the test, the students were asked some demographic questions and subjective questions, such as how much they liked to workshop. 

## Averages

First things first: The game based learners scored better by 2.1 Points (of 25) and had a score of 19.6 compared to the 17.5 of the microlearners. 
Both populations are evenly distributed as a standard gauss-curve. 
This was, pretty surprising! We had the impression that the microlearners had less fun, but were more focussed. This somewhat proves us wrong. Or, does it? 

As I told you in the last post, this was the bachelor-topic of my friend and he was a bit surprised by this as well. 
He looked at the data more closely and applied a formal [student t-test](https://en.wikipedia.org/wiki/Student%27s_t-test). 
A students t-test is used to compare two small events to a hypothesis, for this case my friend choose to use "both workshops are not equally good in improving the grade of students". 
So, in general he was looking to find out whether you can statistically say that game-based learning is better. 

You do some magic math mumbo-jumbo for the t-test, and in fact given the current state of data the hypothesis cannot be rejected. 
For humans: Just because in our two workshops the game-based learners scored better, the difference in score (on average and standard deviation) is too low to not be random. 

While this frustrated my friend (its somewhat unfun to write this in your results), it calms me in my personal perception of the workshops. Math is fascinating. 

## Fun Factor 

On the soft-questionnaire everyone rated the workshops A+, which is about the same flattering as it is useless for comparison. I still appreciate it. 

Another soft-question was how much in touch they were with the topic of testing, which turned out to be not correlated to any performance achieved by either workshop. 
It has to be noted, that most of the attendees hat no to little knowledge at all whatsoever. 

In the same way there were only low correlations of results to questions such as *testing is a hard topic to understand* or *the workshop helped me to understand testing better*. 
To some extend that means that ... the self-perception of the topic does not correlate the test-results. 
So, if you have a bad grade, you should have noticed that testing is a hard topic to follow or that you did not understand it well. This was not the case, which is odd. 

Funny enough, the question *Testing is a tough topic for a workshop* was lower (so it was perceived easier) in microlearning, which scored worse on the knowledge test. Hm. 

The biggest variations in the questionnaire were in asking whether the workshop was *too long*. 
The micro-learners on average said the workshop was not too long, while the game-based learners felt it was too long. 
But both groups had a big deviation in their answers. 

## Conclusion

The overall conclusion is a lot of confusion. 
Most of the things perceived were wrong:

- Microlearning was more structured and *"better"*.
- Microlearners said they understood it better
- Everyone said they were happy with the workshop despite many saying it was too long
- That there must be a *winner* in workshops when it comes to results

The trainers and the attendees all had no clue what they are seeing. 
So, while everyone had a strong oppinion on the questions and results, it turns out that from a statistical point of view nothing can be proven from just these two workshops. 
This can be solved with repeating the workshops, and maybe find a more divers group. As now all the resources are there, it can easily be repeated. 

Until then, I'd go with a very simple metric which workshop to choose: Do the one thats easier (and more comfortable) for you as the trainer. 
And that is by far the micro-learning workshop. 

The reasons why microlearning are pretty straight forward: 

- It needs less moderation/discussion 
- It depends less on the levels of teams 
- There are no team-dynamics which can be bad (There are good team-dynamics too)
- It scales better in the number of participants

I think there is a lot of potential in the game-based approach and it was a lot of fun. 
But in retrospect I think I'd do something like the game-based task for team-building or fun and not for actual learning. 

That's it! Have a nice day everyone. 

## Data 

![datasheet](/assets/images/Workshop_Data.PNG)
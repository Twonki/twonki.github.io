---
layout: post
title:  "How to Speedup your ML Debugging"
date:   2020-12-06
author: Leonhard Applis
description: A little trick to help you in debugging experiments
categories: [Data-Science, Tips & Tricks]
reading: 3
---

Lately, I had the unforgiving task to get someone elses experiment to run.
Reproducible research is ... not as clean as people tend to claim, 
and even if you get things like 'reproducible'-badges my opinion of true reproducibility is a bit different. 

However, crying doesn't help and so I had to get it running. 

There were some issues: 

1. My local desktop was not 'good enough' to run the training in reasonable time 
2. The dependencies were not fully specified, making the script fail at different stages 
3. To run the experiments, I had to move it to a server, increasing the feedback time further
4. The remote server had a different OS then my desktop

In total: Pretty nasty. 
Everything was slowing down my feedback cycle of what is working and what is not, and unfortunately the task was not solely solvable with brainpower, 
but e.g. I had to *guess* which Tensorflow version was used. 
Apart from the effectiveness, it felt really draining to always wait for failing runs. 

<br/>
So the first step was to start fresh from docker, first with hardcoded data later with mounted data. 
This had the benefit that I could rely what is running on my desktop also works on the remote server. 
If my server would not be super-guarded port secured in a vpn etc. I could have run my local docker files directly against the server. 

The second step was to extract some items from the initial run script into earlier steps, so the build can be cached sooner. 
Particularly, I made a little python script that just downloads the required models before the actual code is copied, 
so that the later stages need less time to build. 

The last step was a trick a friend of mine told me: I reduced the training, test and validation files to 3 entries each, to have a rapid run. 
While obviously the results where garbage, I could easily see that there are results and they fit the expected format. 
With the docker-compose it's easy to just run with different files. 

These three steps made my progress much smoother, and the resulting image is nicely reproducible and configurable for everyone, without additional effort. 

<br/>

I think there are more domains where you can apply these schemas, e.g. some webapps that are not containerized. 
If you want to have a container in the end, you might as well just start from the container and save you some time. 

Also, always have nice and smart friends and treat them well, because they often have good advice. :) 
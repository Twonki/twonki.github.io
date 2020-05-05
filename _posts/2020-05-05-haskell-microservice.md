---
layout: post
title:  "Building a Haskell Microservice"
date:   2020-05-05
author: Leonhard Applis
description: A Tutorial on how to build a minimal microservice using haskell and docker
---

Hi! I'll spare you with the mandatory "microservices are very important"-talk and cut right to it :) 

Microservices ARE very important, and one thing I like about them is that you can choose a language you see fit for a single task. 
Most modern Microservice Architectures are full-java + Spring, but I see especially in Microservices a nice way to use haskell or other functional languages. 

It is also a nice way to show your colleagues that it's not that scary, and you are not building an unmaintainable scary beast. 

## Goals and Prerequisites 

I already have a [project on microservices](https://github.com/Twonki/Microtope) and I want to add another one written in Haskell. 

The Haskell microservice shall be a command-line executable, which runs every 30 minutes and performs database queries. 
For my pet project it will check that every user has a logout for every login, so not two consecutive logins, but for the tutorial here we will just query a health table. 
The database is mariadb, and we'll use HDBC+ODBC.

To put it into "production" we will make a docker image for our service and add it to a docker compose. 

I am starting with a ready-made database. 
That is a mariadb database with tables and users (one already for our microservice), easy usable by us via docker. 

That is all we need! 
I'll put some copy and paste code into each section, and the *final* version of this minimal microservice can be found at [this tag](www.todo.com).

## Step by Step 

### Dummy Console-App in Docker 

### Console Arguments & Connection-properties 

Note: Talk about Configuration-files vs. Arguments

### Add HDBC & ODBC outside of Docker 

### Add ODBC to Docker 

Note: talk about issues of versions

### Add "Wait for it" and pass command args 

### Make a Minimal Docker-Compose

## Lessons learned 
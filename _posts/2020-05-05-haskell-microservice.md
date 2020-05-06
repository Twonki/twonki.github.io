---
layout: post
title:  "Building a Haskell Microservice"
date:   2020-05-05
author: Leonhard Applis
description: A Tutorial on how to build a minimal microservice using haskell and docker
---

Hi! I'll spare you with the mandatory "microservices are very important"-talk and cut right to it :) 

Microservices ARE very important, and one thing I like about them is that you can choose a language you see fit for a single task. 
Most modern microservice architectures are full-java + Spring, but I see especially in microservices a nice way to use haskell or other functional languages. 

It is also a nice way to show your colleagues that it's not that scary, and you are not building an unmaintainable scary beast. 

## Goals and Prerequisites 

I already have a [project on microservices](https://github.com/Twonki/Microtope) and I want to add another one written in Haskell. 

The Haskell microservice shall be a command-line executable, which runs every 30 minutes and performs database queries. 
For my pet project it will check that every user has a logout for every login, so not two consecutive logins, but for the tutorial here we will just query a health table. 
The database is MariaDB, and we'll use HDBC+ODBC.

To put it into "production" we will make a docker image for our service and add it to a docker compose. 

I am starting with a ready-made database. 
That is a MariaDB database with tables and users (one already for our microservice), easy usable by us via docker. 

That is all we need! 
I'll put some copy and paste code into each section, and the *final* version of this minimal microservice can be found at [this tag](www.todo.com).

## Step by Step 

Every good thing needs a name, and my app is called *wesir*. I just liked it, and I didn't want to pseudonymize the files so I hope you like it to :) 

Lets build the app step by step with some examples. I might miss out some files / tweaks but they are definitely in the repository.  

### Dummy Console-App in Docker

So the first part is having a minimal program to greet us:

```Haskell
module main where 

main = do 
    print "Hello World" 
```

and we have a normal cabal file alongside, where we also added all of our dependencies: 

```Haskell
cabal-version:       3.0
name:                Wesir
version:             0.0.1.0
synopsis:            Program to check MicroTope Database validity
license:             MIT
category:            executable

executable Wesir
  import: deps
  main-is:            Program.hs
  build-depends:  base                  >= 4.13.0 && < 4.14,
                  text                  >= 1.2.4 && < 1.3,
                  HDBC                  >= 2.4.0.3 && <2.5,
                  HDBC-odbc             >= 2.6.0.0 && <2.7,
                  optparse-applicative == 0.15.1.0,
                  time
  hs-source-dirs: Src
  default-language:    Haskell2010
```

we check it with `cabal new-build` and see our "Hello World" with `cabal new-run`. 

If thats working fine, we can add a minimal docker file: 

```Docker
FROM haskell:8
RUN cabal update
WORKDIR /App
COPY . .

RUN cabal new-install 

# TODO: Add parameters as required

ENTRYPOINT ["Wesir"] 
```

With this, we do a short `docker build . -t wesir` and `docker run wesir` - and we should see another Hello world. 


### Console Arguments & Connection-properties 

For the next step we could either try to make a first connection with hardcoded properties, or to pass properties. 
I decided to go for passing parameters first. 

There are two common approaches to configure your service: A configuration file or passing command-line arguments and environment variables. 

There are benefits and downsides to each which should be taken into consideration. If you are using environment variables, some applications will turn into very cryptic and overloaded commands (once you are reaching the second line in args, it looks scary to strangers). On the other side, if you are using files you have to carefully manage all your files and have a nice way to distribute them.
Managing secrets is another hard topic for both approaches. 

If you already have an architecture, my suggestion is to stick to what is already in place. 
For my case I use environment variables in my docker compose and invoke the services with command-line arguments. 
The scope of the services made it reasonable, and also this example service only needs parameters for our Database connection.

For handling command line arguments in Haskell I recently came across [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) which offers a particularly great tutorial. 
Using applicatives, it enables us to write small parsers for a datatype which resembles the required arguments. 

Our datatype looks like this: 

```Haskell
data ConnectionProperties = Connection {
    host :: String  
  , port :: String  
  , user :: String
  , password :: String
  , database :: String
}
```

And the parser is mostly adjusted from the readme of optparse: 

```Haskell
    Connection
        <$> strOption
            ( long "host"
            <> short 'h'
            <> help "address of the database to connect to"
            <> showDefault
            <> value "127.0.0.1"
            <> metavar "STRING" )    
        <*> strOption
            ( long "port"
            <> short 'p'
            <> help "port of the database to connect to"
            <> showDefault
            <> value "3306"
            <> metavar "STRING" )    
        <*> strOption
            ( long "user"
            <> short 'u'
            <> help "The username that will connect to the database"
            <> showDefault
            <> value "admin"
            <> metavar "STRING" )    
        <*> strOption
            ( long "password"
            <> help "The password to use to connect to the database"
            <> value "admin"
            <> metavar "STRING" )
```

We added a short explanation and a default value for each argument. 
If no attribute is given, the default is taken, and if something strange is invoked a help is printed showing the expected datatype and description.

To use this cool feature, we adjust our main: 

```Haskell
module main where 

import Options.Applicative
import Data.Semigroup ((<>))


printArgs :: ConnectionProperties -> IO ()
printArgs (Connection h p u _) = 
    print "Starting Wesir, connecting to "++h++":"++p++" as "++u

main = do 
    args <- execParser opts
    printArgs args

-- this function prints the help, if no parser successfully matches
opts = info (connectionPropsInput <**> helper) (fullDesc<> progDesc "TODO: add bottom text description"<> header "TODO: Add top text description")

-- Code from above
-- ...
```

That's all we need for now. 

We can invoke our app with `cabal new-run` and it'll greet us with default values, or we can run `cabal new-run -h 172.16.0.2 -u anita -password hello123`. 

Perfect! This is all we want for now. 

A real application should have some more args to be fair: Whether we want to be verbose, the logging level and a logging directory would be good candidates. 

For my example I additionally have a parses that either looks for the quintett of connection properties OR for a connectionstring. 
That was quite easy with optparse-applicative.

### Add HDBC & ODBC outside of Docker 

### Add ODBC to Docker 

Note: talk about issues of versions

### Add "Wait for it" and pass command args 

### Make a Minimal Docker-Compose

### Bonus: Github CI 

## Lessons learned 
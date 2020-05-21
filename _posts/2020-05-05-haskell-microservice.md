---
layout: post
title:  "Building a Haskell Microservice"
date:   2020-05-09
author: Leonhard Applis
description: A Tutorial on how to build a minimal microservice using haskell and docker
---

Hi! I'll spare you with the mandatory "microservices are very important"-talk and cut right to it :) 

Microservices ARE very important, and one thing I like about them is that you can choose a language you see fit for a single task. 
Most modern microservice architectures are full-java + Spring, but microservices are a nice way to utilize haskell or other functional languages where they are fit. 

It is also a nice way to show your colleagues, that it's not that scary and you are not building an unmaintainable, scary beast. 

## Goals and Prerequisites 

I already have a [project using a microservice architecture](https://github.com/Twonki/Microtope) and I want to add another service written in Haskell. 

The Haskell microservice shall be a command-line executable, which runs every 30 minutes and performs database queries. 
For my pet project it will check that every user has a logout for every login, so not two consecutive logins, but for the tutorial here we will just query a health table. 
The database is MariaDB, and we'll use HDBC+ODBC.

To put it into "production" we will make a docker image for our service and add it to a docker compose. 

I am starting with a ready-made database. 
That is a MariaDB database with tables and users (one already for our microservice), easily usable with docker. 

The steps required to install mariadb-odbc are shown in the regarding section below. 

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
                  optparse-applicative == 0.15.1.0
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

ENTRYPOINT ["Wesir"] 
```

With this, we do a short `docker build . -t wesir` and `docker run wesir` and we should see another Hello world. 


### Console Arguments & Connection-properties 

For the next step we could either try to make a first connection with hardcoded properties or to pass the required connection-properties. 
I decided to go for passing parameters first. 

There are two common approaches to configure your service: A configuration file or passing command-line arguments and environment variables. 

There are benefits and downsides to each which should be taken into consideration. If you are using environment variables, some applications will turn into very cryptic and overloaded commands (once you are reaching the second line in args, it looks scary to strangers). On the other side, if you are using files you have to carefully manage all your files and have a nice way to distribute them.
Managing secrets is another hard topic for both approaches. 

If you already have an architecture, my suggestion is to stick to what is already in place. 
For my case I use environment variables in my docker compose and invoke the services with command-line arguments. 
The scope of the services made it reasonable, and also this example service only needs parameters for our Database connection.

For handling command line arguments in Haskell I recently came across [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative), which offers a particularly great tutorial. 
Using applicatives enables us to write small parsers for a datatype which resembles the required arguments, in our case the connection properties. 

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
            <> help "address of the database-server to connect to"
            <> showDefault
            <> value "127.0.0.1"
            <> metavar "STRING" )    
        <*> strOption
            ( long "port"
            <> short 'p'
            <> help "port of the database-server to connect to"
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
        <*> strOption
            ( long "database"
            <> short 'd'
            <> help "the database name to connect to at the database server"
            <> value "microtope"
            <> metavar "STRING" )
```

We added a short explanation and a default value for each argument. 
If no attribute is given the default is taken and if something strange is invoked a help is printed showing the expected datatype and description.

To use this cool feature, we adjust our main: 

```Haskell
module main where 

import Options.Applicative
import Data.Semigroup ((<>))

printArgs :: ConnectionProperties -> IO ()
printArgs (Connection h p u _) = 
    print "Starting App, connecting to "++h++":"++p++" as "++u

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

For my example I additionally have a parser that either looks for the quintett of connection properties OR for a connectionstring. 
That was quite easy with optparse-applicative. 
I can highly recommend this library - it does all I want and I can throw around some applicative operators to impress the readers of my blog. 

### Add HDBC & ODBC outside of Docker 

The next step will be to connect to our database outside of the docker image using HDBC - that is from `cabal new-run` directly. 

First it's required to spin up the database (and wait a bit). For these initial steps I used `docker run -p 3306:3306 database` so I have it on localhost available. 

To use mariadb-odbc from a linux machine you can follow [this tutorial from mariadb]([install mariadb-odbc](https://mariadb.com/kb/en/about-mariadb-connector-odbc/)).
you first have to install a bunch of general odbc-utilities via `sudo apt-get install unixodbc unixodbc-dev unixodbc-bin` and then install the mariadb-specific odbc with 

```
mkdir odbc_package
cd odbc_package
wget https://downloads.mariadb.com/Connectors/odbc/connector-odbc-3.1.7/mariadb-connector-odbc-3.1.7-ga-debian-x86_64.tar.gz
tar -xvzf mariadb-connector-odbc-3.1.7-ga-debian-x86_64.tar.gz
sudo install lib/libmaodbc.so /usr/lib/
```

Check your correct package and the current version first [here](https://downloads.mariadb.com/Connectors/odbc/).

This will give us some issues later, but lets not be frustrated right now, because at the moment everything is working. 

To use our driver easily with Haskell, we need to register it on our machine. We do so by defining a driver template: 

```
[MariaDB]
Description = MariaDB Connector/ODBC v.3.1.7
Driver64 = /usr/lib/libmaodbc.so
Driver = /usr/lib/libmaodbc.so 
```

And apply it with `sudo odbcinst -i -d -f Resources/MariaDB_odbc_driver_template.ini`. We can inspect it with `odbcinst -q -d`.

This is all the changes we need to make to our machine and we can go back to Haskell. 

We can add the hdbc now for our Program: 

```
module Main where

import Database.HDBC.ODBC
import Database.HDBC

-- Other Imports 
-- [...]

-- A simple helper that builds a connectionstring from our arguments, and uses it to establish a connection
getConnection :: Arguments -> IO Connection
getConnection Connection h p u pw d = 
    let cs = "DRIVER={MariaDB};SERVER="++h++";PORT="++p++";USER="++u++";PASSWORD="++pw++";DATABASE="++d
    in connectODBC cs 

main :: IO ()
main = do 
    args <- execParser opts
    printArgs args

    conn <- getConnection args
    results <- quickQuery conn "SELECT * FROM health;" []

    putStrLn "Service Health: \n \n"
    print results

    -- TODO: properly close connection

    putStrLn "\n\nbye, have a great time!"

-- Other Code from above
-- [...]

```

This will do. 
If the database is successfully running on localhost and the standard port, we can verify it using the standard parameters and do `cabal new-run`.

Otherwise if we hide it, or have a different user, we need to pass arguments such as `cabal new-run -u otherUser --password otherPw`. If you use my image, the user can be found in the [sql files](https://github.com/Twonki/Microtope/blob/master/database/sql-files/Step02_createUser.sql).

### Add ODBC to Docker 

Now we are entering the pain-point. 
On my normal machine, everything was working great (that is, a normal desktop ubuntu and many common things already installed). 
Something on this made everything working perfectly fine and reasonable. 

But when I tried to reproduce the steps above in the DockerImage, it proved to not work. At all. 
Likely the reason is that some dependencies are missing, as the Haskell base-image seems to be a minimized debian image. 

That is perfectly fine in general, but chasing the exact missing dependency of a certain mariadb-odbc connector for a specific minimized debian showed to be quite intense. 

Being honest: I have not resolved it. But i mitigated it, using an earlier version of the mariadb connector. 

After that, it was simply reproducing the steps above and the new docker file looks as follows: 

```
FROM haskell:8

RUN apt-get update

# Drivers for ODBC, wget for mariaodbc download later
RUN apt-get install wget unixodbc unixodbc-dev unixodbc-bin -q -y

# Installing Specific MariaDB ODBC
# Taken from https://mariadb.com/kb/en/about-mariadb-connector-odbc/
WORKDIR  /odbc_package
RUN wget https://downloads.mariadb.com/Connectors/odbc/connector-odbc-2.0.19/mariadb-connector-odbc-2.0.19-ga-debian-x86_64.tar.gz
RUN tar -xvzf mariadb-connector-odbc-2.0.19-ga-debian-x86_64.tar.gz
RUN install /odbc_package/lib/libmaodbc.so /usr/lib/

RUN cabal update

WORKDIR /App
COPY . .

RUN odbcinst -i -d -f Resources/MariaDB_odbc_driver_template.ini

RUN cabal new-install 

ENTRYPOINT [ "Wesir"]
```

Make sure to adjust the version in the MariaDB_odbc_driver_template. 

To verify this step do `docker build . -t wesir` and `docker run wesir`. 

It will fail, but if it tells you that you have no mariadb on localhost than it's failing the intended way. 

We fix this by passing the correct arguments into the docker image at runtime, which will be our next step. 

### Add "Wait for it" and pass command args 

So one way to pass the args is to set the entry point of docker when running. 
But that's not the proper way to do it in my opinion. 
A nicer way is to set environment variables in the dockerfile and invoke the command with the regarding parameters. 

We do by adding the following to our Dockerfile: 

```
ENV MariaDB_Adress 127.0.0.1
ENV MariaDB_Port 3306
ENV MariaDB_DatabaseName microtope
ENV MariaDB_User admin
ENV MariaDB_PW admin

ENTRYPOINT [Wesir -h ${MariaDB_Adress} -u ${MariaDB_User} --password ${MariaDB_PW} -p ${MariaDB_Port} -d ${MariaDB_DatabaseName}"]
```

This for example enables us to pass the password as a [docker-secret](https://docs.docker.com/engine/swarm/secrets/), which can be considered best practice. Same for Kubernetes.

However if we start with a fresh instance of mariadb there is a little tweak: 
Just writing "depends on" and declaring the mariadb will cause an issue. The reason is that the mariadb container says its ready the moment the mariadb engine is up. 
That is not the point where the mariadb has initialised its databases and accepts requests. 
So we need to wait for it. We can do so by adding sleep and retry commands to our images, but a good way to deal with the issue is ["wait for it"](https://github.com/vishnubob/wait-for-it).

It makes a very basic (and usually invalid) ping-command on any host and port you want, and if it gets rejected it starts another specified shell command. 
The repository is well documented, but we need only basic functionality. 

To use it, we download the shell-file and add it to our resources. 
Then we only have to adjust entrypoint: 

```
 ENTRYPOINT [ "/bin/bash", "-c","./Resources/wait-for-it.sh -t 0 -s --host=$MariaDB_Adress --port=$MariaDB_Port -- Wesir -h ${MariaDB_Adress} -u ${MariaDB_User} --password ${MariaDB_PW} -p ${MariaDB_Port} -d ${MariaDB_DatabaseName}"] 
```

The used flags are `-t 0` to "never stop trying" (quite motivating, otherwise it would end after trying for 100s) and `-s` to be silent. 
To specify host and port we can re-use our environment variables. 

Thats it! We can see it in full glory now. 

### Make a Minimal Docker-Compose

With all the steps we needed up to this point, our compose is dead simple: 

```
version: '3.3'

services:
  db:
    image: microtope/database
    environment:
      MYSQL_RANDOM_ROOT_PASSWORD: "yes"
      TZ: "Europe/Berlin"
  wesir:
    image: microtope/wesir
    environment:
      MariaDB_Adress: db
      MariaDB_Port: 3306
      MariaDB_User: admin
      MariaDB_PW: admin
    depends_on: 
      - db
```

We run it with `docker-compose . up` and stop it properly with `docker-compose . down` (Don't forget to stop it). 

to start it regularly we need to use docker-stack (the options for `deploy` are not available in compose). 

To start it in a regular fashion, including restarting it, the ugly way is to add a cronjob to our image. 
But we can use functions of docker-stack or maybe kubernetes. For docker, we add: 

```
    deploy:
      restart_policy:
        condition: any
        delay: 10s
        max_attempts: 3
        window: 30s
```

Add this snippet on the same level as `environment` to our apps configuration to provide a *deploy* specification.
The time-window is on 30s (not 60m) to show that its working a bit faster.

To set up a localhost docker stack, do:

`docker stack init --advertise-adr 127.0.0.1`

then you can do

`docker stack deploy -c . teststack`

clean up with

`docker stack rm teststack`

The docker stack can also be on a remote host, which enables us to directly deploy our configuration. 

## Conclusion 

So in general I noticed that it's not too hard to put Haskell in my existing project. 
I'd say there is no point against Haskell in regards of infrastructure - it is as hard as any other language. 
How good or horrible Haskell is for your project depends on the task and on what kind of code you write. 

The only pain-point were the mariadb-odbc issues, but to be honest they were reasonably fast resolved (~4h) and I might would have faced the same issue with other languages. 
I hope that this post helps some people, as I have not seen any recent example on how to use mariadb with odbc in docker for Haskell.  

Another takeaway was that docker proofed to be very valuable in showing me the odbc issues. 
Without docker, i'd just have clapped my hand and say "Yeah i'm done" too early. 
Also, Docker enables me to deliver a full artifact that is not any different. 

But a last word of warning: 
Due to cabal the builds take quite long. 
In Docker for most CIs these steps are not cached, and i'd be very careful to use it carelessly if you pay per build-minute.
I have not yet figured out how to properly only install dependencies when they change so they can properly cached, as every time I touch the cabal file the layer changes. This is maybe a topic for the next post. 
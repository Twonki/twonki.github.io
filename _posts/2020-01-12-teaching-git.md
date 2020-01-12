---
layout: post
title:  "The Git-Tutorial-Tutorial"
date:   2020-01-12
author: Leonhard Applis
categories: [Teaching, Tipps, Tutorial]
description: A summary of my lessons learned from teaching Git 
---

Mostly by accident I have become the *GoTo*-person in my department when it comes to git.

Whether a new trainee needs an introduction or a team moves away from subversion, usually my phone rings.

That's not bad, just sometimes a little repetitive.
With more than 15 runs on explaining git to usually former untainted people, I thought I might share what worked well explaining git, and what didn't.
*Working well* is roughly defined as "number of questions I got afterwards about git"+2x"number of git-problems someone produced for the team".

<br>
**This is not a git-tutorial!** This is a git-tutorial-tutorial.

### History of Software Collaboration

Sometimes I started with a little overview about how software was made, starting with punched-cards and that a patch was a literal patch on the card.

I then went over to the dreaded "Friday-afternoon-hard drive" which was going around and one developer after another merged his changes on this disk, praying for a working product,
because the person merging before usually left for the weekend.
The very same hard drive got on Monday handed to every developer and every tester.

<br>
After that I went to distributed version control, usually centralized thanks to the internet with subversion and git.
With this introduction it's usually easy to see why a distributed solution with versioning is beneficial.
People understand the need for something like git or subversion.

<br>
But: Is it helpful?

<br>
For what I've seen this introduction has some values. First it's a nice start to chat about the topic, to show some pain other people had is always heart warming.
Its also a good start to question what the person is currently doing, e.g. if they have a Dropbox with Word Documents labelled *doc_v1, doc_v2, doc_final , doc_FINAL, doc_leonhard_v2*.
Sometimes this talk helps people to understand the need for version control, or for centralized code. This is usually the case with older colleagues who do not *want* to change the way they work. Another charming argument is to ask them if they ever had problems with no-version control and if they might have liked it otherwise.

<br>
Does it help trainees?

<br>
In my opinion - mostly not. They do not know how people worked together earlier, and they usually don't care.
They enter the ring with cutting edge technologies, and want to get their hands on something cool.
Therefore its more important to show them why git is also *cool*, and not only a corporate guideline they have to follow.
Additionally, I am not so old myself that I've seen punched-cards in action, so maybe I am not so authentic when it comes to story telling.

### Drawing the commit-graph

One of the best improvements is to draw a classic git-commit graph while explaining all the methods and functions.

I try to stick to git-terms such as commit, merge, branch etc. and try to show it on a living graph.

Part of the success is that I draw the graph anew, so I do not hit my poor fellow with 2 DinA4-Pages but I start with the very first initial commit.

<br>
I feel like this makes the big difference between *understanding git* and *learning git commands*.

<br>
One thing that came in handy was collecting all important terms below the graph and let the sheet of paper itself be a cheat-sheet - not so much for commands but for the ideas.
For this to work you need a good handwriting.

Whether it turns out to be nice or rather crude, the commit graph can always come in handy to solve problems.
Because the problem (if there is any in git) can usually visualized with the commit graph.
<br>
E.g. if there is a more complex structure of branches and the way they need to be re-merged can be shown nicely.
More complex ideas like rebasing and cherry picking can easily be shown with a graph (for cherry picking you can draw your graph with cherries and lemons instead of circles - everyone likes that).

### Commit as a "safe-game"

One analogy I often use is that making a commit is like *saving* in a video game.
And that it makes sense to safe before you choose a class or spell to potentially checkout another one later.

This is especially popular with trainees, older colleagues (except for older *gamers*) find this not a helpful comparison.

<br>
In general I will stick to this idea, but I also think it's lacking a little bit.
The safe game analogy does not take in account merging, which is somewhat one of the most important advantages of git.

The idea of merging two safegames would be cool in a lot of games, e.g. Diablo.

### Git-Flow & Trunk-based Development

Usually after everything technically git related I talked about workflows and team-collaboration with git - namely feature-branches, name-branches or trunk-based development.

<br>
I especially mention a good story for named branches, where we clearly stated that everyone has exactly one branch, and on the next team meeting we had a branch *Deepak* and *Deepak2*.
While my supervisor started ranting about that this was truly, and honestly, the easiest command he ever gave us and we still failed, I friendly mentioned that we had two people with first name Deepak.
The idea of picking last names failed right after due to two *smiths*.

While this is one of my favourite stories of this project, I think the general discussion about the workflow is not good for a first introduction.
Also it is a giant, semi-religious discussion in general.

<br>
I would shortly show the way it is done in the current or aimed project, give short reasons why this is beneficial and just move on.
If you do not have any reasons why it is good, you may should consider a different workflow.

<br>
Talking about workflows while learning git is like talking about organizing ride-shares while making your drivers licence.

### cheat-sheets

There are some great cheat-sheets for git out there, and everyone uses them.
I have 0 argument against cheat-sheets, as they make either no difference (but also no effort) or a difference for the better.

<br>
Some thoughts on choosing the right cheat-sheet:

- If you are using a remote-git-platform, consider using a specific cheat-sheet of your provider (e.g. Azure, GitHub, BitBucket)
- Consider your teams workflow, so if you are doing trunk-based you may can spare out a lot of branching-utilities
- Consider printing in colour or not printing in colour - if you want to pin it somewhere and your corporate colour is blue, having a yellow cheat-sheet is nasty
- Consider making your own cheat-sheet, including information how to get through company VPN, use your company account, links to company-wikis, people to ask for help etc.

What greatly improves cheat-sheets is looking over them together.
So don't just throw it at the people and walk away, but look at the cheat-sheet and go over everything on there, ask and answer questions.

This is also a nice place to repeat the things you discussed earlier, e.g. pointing at the right places in the commit-graph.

### Literature

Handing out literature proofed to be rather unused.
I once acquired the *Git Pocket Guide* and handed it out to the current learning person or team.

I usually found the book at the exact same spot where I placed it, sometimes with some dust on it.

<br>
Thing is that those books are impressive, and have a lot of things in it.
The very basics of Git (=the parts you need to be effective), are way to little for a book.

Therefore hard things are in there such as cherry picking, rebasing and general Hokuspokus on the history.

Those are only confusing for a beginner, and as far as I've seen there is a git-guru somewhere to care for the voodoo if needed (maybe that's you?).

<br>
The few times someone used the pocket guide they rather produced scary monstrosities with warped and twisted histories.
Do not give those techniques to beginners unless you really want to have a fun day.

Oh, as literature also count for me all the online tutorials which would be printed longer than 4 pages.

### Console vs. GUI

There are a lot of GUI-Tools for git (such as TortoiseGit,GitKraken or GitHub Desktop) or good IDE-Integrations (for example VS, VSCode or IntelliJ) and there are IDE-Integration (for example Eclipse).

Especially people coming from SVN are used to TortoiseSVN and have a better time adopting Git with TortoiseGit.

Visual Studio (if connected to a TFS or azure repo) does a lot of magic for you and can be very productive.

<br>
I think for learning the *technology git* it is better to learn it once on the console.

I do not say to stay away from the "nicer" tools forever - just for learning purposes.
You should learn Java before you go to Spring.

<br>
In addition, git is in general a good first experience for someone ever working on the console.

<br>
I had really hard problems with people working from GUIs, some of them were:

- working on the wrong branch
- merge or discard wrong items (usually by clicking fast some accept buttons)
- lacking the skill to get changes from one branch to another
- double reverting and resetting commits
- lacking skill to turn to the code "as of February"

Most of these problems do not occur if someone learned it from the console as far as I can tell.

If someone truly understood how does the history and checkouts work, they are usually able to re-enter a state where they can help themselves.

Especially when a whole team moves to Git+GUI at once, the git history turns into a cruel cacophony usually scarred with commit messages such as "merge","fix", "f*** git", "changes".

<br>
If any GUI tool is used, a short introduction should be provided.

### 1on1 vs. 1onN

From my experience explaining git to teams ends up in either:

1. A wall of silence, no one has questions, everyone is fine
2. Everyone is really hyped and does a lot of bogus (And at least 3 days of e-learning tutorials + certification)

I am not sure why this is the case - but I guess its because Git just gets in line with every other technology you hear about every other day that "will be the next big thing".

If someone is giving a group a talk about a technology it always blurs and ends up to be the very same: "Problem X"-"Solution Y"-"The world is saved!"

That's not bad, that's how you give a talk.

But I think this pattern does not apply very well to learning technologies.

If you are at a conference and everyone has chosen to be in your talk it might be exactly what you want - but not for your department.

<br>
Teaching someone personally has of course the drawback of taking more time, but you can put more focus on really helping him understand.

Git is a tool, and using it is essential.
This is only achievable in a 1on1.

<br>
I would have better given everyone a nice 1v1-Intro than giving a 1vN and later have all the problems to fix.

Interesting enough, as far as I can tell *fixed* git-problems do not reoccur. I have been rarely asked the same question from someone twice.

### Miscellaneous

**Commit Templates** are a nice thing in general, as they kill the habit of `git commit -am "fixed typo"`, `git commit -am "changes"`.
While I think *enforcing* them is rather draconic, having them is a good thing.
Setting them up with the trainee, and using them once or twice while learning is fine.
Maybe you want to show the commit history of your project, to show why this is useful (be careful to not play a blame-game here).

<br>
**GPG & SSH** are very important features for security and authentication.
I would generally recommend for any non-open-source company-code to only accept signed commits in their projects.

Sadly - some platforms (such as Azure) do not support GPG-signing. Hopefully this will change.

Setting up both keys usually needs to be done only once (per machine) and fits well in the "git onboarding"-process.

<br>
**.gitignore** is your friend. In general I recommend [gitignore.io](www.gitignore.io) and manually write it somewhere on the cheat-sheet. 

Never give a git-tutorial without mentioning gitignores! 

### Conclusion

In general when it comes to teaching Git my experience shows:
Stick to the basics, take time for the basics, keep it low on assets and make sure that everyone understands the need for distributed version control.
Never forget to show your apprentices why git is good *for them* - they should never use a tool just to please you or some faceless corporate rule.

Git is a nice tool to learn teaching in my opinion.
It is complex enough to not be trivial, while it is easy enough (and friendly enough) to be understand by anyone.

<br>
Git is either part of a new beginning or of a transformation, so the success or failure of your teachings are soon visible.

<br>
In addition it helps you to remember the basics and beginnings for yourself.
Once you are used to git, you are like a sleepwalker doing your commits and merges as routine.
The idea of someone *failing* at something "so easy" is at first puzzling.

<br>

Teaching git is a good start for someones git and your teaching.

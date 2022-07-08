# Setup
## Installing Haskell

I *strongly* recommend using [GHCUp](https://www.haskell.org/ghcup/) to install Haskell and Cabal.

I developed this project using GHC version 8.10.7, and Cabal version 3.6.2.0, so you probably want to use the same versions to test/run it.

## Building

First, run `cabal update` to get the latest list of packages from Hackage.

Run `cabal build` in the root directory of the project to build it. This may take a while on first build, as you won't have any of the dependencies installed or built yet.

## Running

Use `cabal run server` to start the web server.

Use `cabal test` to run unit tests.

# Project Description

The project is (mostly) not particularly complicated. Both the frontend API (i.e. the one the server serves) and the backend APIs (CSCards and ScoredCards) are modelled as individual Servant APIs. 

When the server receives a request, it converts the data in the POST into the relevant format for each of the backend APIs, calls them all in parallel, collects the results, converts them back into the expected output format, and then sends them to the caller.

Failure of any of the backend APIs is considered non-erroneous, so in the event that one fails and the others succeed, we still send the data from the successful calls to the caller.

If a backend API is taking to long (nominally 5 seconds), we consider that a failure and skip it.

The only "clever" thing I'm doing is some automatic inference to work out which request/response types correspond to which of the backend API endpoints, so that we can do the conversion between the frontend and backend types automatically (and type safely!). More details on that in `src/ClearScore/Types/Request.hs`.

Additionally - and I've already conveyed this via email, but I'll mention it here as well - there is a bug in your backend APIs (the CSCards and ScoredCards ones) whereby they fail with a security error from CloudFlare if the `User-Agent` header is not set. You should really fix that (or at least note it in the documentation for the assignment), as it is _very_ non-obvious that that would be the case, and took me the better part of a day to discover that that was the root cause of the issue I was having.

As far as deploying this, I'd probably default to running it in a Docker container in some cloud or another. It would also _probably_ not take too much modification to run it in a Lambda or similar, but currently it's not set up to do that.

# Testing / QA

I've mostly favoured type safety over unit tests for this. I typically consider properly designed type-safe code to be preferable to unit tests - mostly because the compiler checks it, and the compiler has a logic engine, so you can encode a lot of information in types that's either not easily unit testable, or which is easier to maintain at the type level.

Once you get passed the type-safe stuff, there isn't actually that much left that I consider to be worth unit testing. If I was going to dedicate more time to this, I'd probably have a go at mocking each of the backend servers, but I've already sunk quite a bit of time into this. Happy to talk through what I'd do there in an interview.
# bolton

Command line app to manage Haskell apps installed from hackage

## Installing Haskell Command Line Apps

### The Old Crap Way

When it came to installing command line apps from Hackage, I used to do something like this:

```
$ mkdir foo
$ cd foo
$ cabal sandbox init
$ cabal install foo
$ mv .cabal-sandbox/bin/bar ~/bin
$ cd ..
$ rm -rf foo
```

(`~/bin` was just a dir in my PATH where I would put manually built command line apps)

Since it used a cabal sandbox this was nice in that it kept my user level cabal package db clean, however it still had one major drawback.

There are some packages that have binaries that rely on static paths in the cabal sandbox it was built in.

`hoogle` is a good example of this, observe:

```
$ mkdir hoogle
$ cd hoogle
$ cabal sandbox init
$ cabal install hoogle
$ ls -lhrt .cabal-sandbox/share/x86_64-linux-ghc-7.8.3/hoogle-4.2.36
total 8.0K
drwxr-xr-x 3 user user 4.0K Nov 23 23:41 databases
$ hoogle data
....
$ ls -lhrt .cabal-sandbox/share/x86_64-linux-ghc-7.8.3/hoogle-4.2.36
total 8.0K
drwxr-xr-x 2 user user 4.0K Nov 23 23:38 resources
drwxr-xr-x 3 user user 4.0K Nov 23 23:41 databases
```

(If you were going to repeat this experiment, the `ghc` and `hoogle` versions might differ)

So when you tell it to download its package database it downloads it to `.cabal-sandbox/share/x86_64-linux-ghc-7.8.3/hoogle-4.2.36/databases`.

This is a directory in the cabal sandbox that under my previous method was getting wiped.

There are other similar such apps that are more critical to the typical Haskell build pipeline that also use hardcoded static paths into the cabal sandbox with which they were built.
(Before sandboxes, these would have been in `~/.cabal/share`).

### A Better, Correct Way

So this is the process prescribed to me (or at least *one* of them, but its been the most reliable one so far) when installing a Haskell app, and it solves the problem decsribed above.

1.  Create a directory `~/haskell-bins/`
2.  Create a directory named `~/haskell-bins/bin`
3.  Add `~/haskell-bins/bin` to my PATH

Then with every app I want to install:

(assume the app I am trying to install is from a package named foo with a binary named bar)

```
$ cd ~/haskell-bins
$ mkdir foo
$ cd foo
$ cabal sandbox init
$ cabal install foo
$ cd ../bin
$ ln -sf ~/haskell-bins/foo/.cabal-sandbox/bin/bar
```

This isn't too complicated a process, but its still a bit much to get through to a beginner in an IRC channel who just wants to get their environment setup to write some Haskell!

So `bolton` is supposed to create something like this `haskell-bins` dir for you and manage the above process and allow you to install/uninstall apps and list the ones you have installed etc...

### The Bolton Way

With `bolton` the process is reduced to:

```
# One off
$ bolton init
Successfully created bolton store
Make sure '/home/user/.bolton/bin' is in your PATH
# add `~/.bolton/bin` to your PATH
$ bolton install-hackage -p yaml
Creating Cabal sandbox..
Installing yaml...
Package Installed.
Package Installed Successfully: yaml (Source: Hackage (version: 0.8.9.3)): yaml2json, json2yaml
```

So its one command to set it up initially and then one line to build and setup an app pulled from Hackage.

(In the future, it should be able to install them from github too).

## Installing Bolton

You can install it from this repository by cutting and pasting the following into your command line:

```
curl https://raw.githubusercontent.com/domdere/bolton/master/etc/bolton-install-github.sh | bash -eu
```

Or you can take a look at what the script is doing [here] [install-from-github-script] and do it yourself.

This will leave the `bolton` binary at `~/bin`, either add `~/bin` to your PATH or move it to your preferred location (that is in your PATH).

## Building the project

Install the dependencies first with either:

    cabal install --only-dependencies

If you do not wish to build tests or benchmarks, or:

    cabal install --only-dependencies --enable-tests

If you want to be able to build the tests, or:

    cabal install --only-dependencies --enable-benchmarks

If you wish to build the benchmarks.

The project must be "configured" at least once everytime `bolton.cabal` changes, this can be done with:

    cabal configure

If you wish to run the unit tests you will have to run:

    cabal configure --enable-tests

If you wish to run benchmarks you will have to run:

    cabal configure --enable-benchmarks

At the moment there are issues with using both flags at the same time.  Its recommended that you use one flag at a time, use `cabal-dev` or `cabal sandbox` 
(see below), and clear your sandbox when switching configurations from one to the other

Then finally build it with:

    cabal build

See `cabal build --help` for more build options.

## Running Unit Tests

**After** running `cabal build`, you can run the unit tests with the command:

    cabal test

## Adding Unit tests

Unit tests are written with [**doctest**] [doctest-github], for instructions on how to add unit tests
see the **doctest** [**User Guide**] [doctest-userguide].

Currently only files in the `src/` directory are searched for tests, it is assumed that the code in `main/`
is a thin layer of code that uses modules from `src/`.

## Running Benchmarks

**After** running `cabal configure --enable-benchmarks` and `cabal build`, the following command will run the benchmarks:

    cabal bench

For newer versions of `cabal`, `cabal bench` will run a `cabal build` automatically if necessary..

## Development: Cabal Dependency Hell?

Cabal's great, but its got its own warts, and when you are developing a few different projects with their own dependency chains, sometimes installing all your libraries to the same place causes problems,

### Cabal version < 1.18

Consider trying [`cabal-dev`] [cabal-dev].  Install it with `cabal install cabal-dev`

In terms of using it, all thats required is replacing `cabal` with `cabal-dev` in all the above command lines.

It will download and install all the dependencies for your project and install them in a `cabal-dev/` directory in your project directory, and they will only be used for this project.

### Cabal version >= 1.18

Cabal version `1.18` and onwards supports sandboxes, which is basically the same idea as `cabal-dev`.

In terms of using it all the commands remain the same, just run `cabal sandbox init` in the root directory of the project before running any of them.

------

The related `cabal-dev` and `sandbox` artifacts are already contained in the `.gitignore` file.

[cabal-dev]: https://github.com/creswick/cabal-dev "creswick/cabal-dev on GitHub.com"
[doctest-github]: https://github.com/sol/doctest-haskell "sol/doctest-haskell on GitHub.com"
[doctest-userguide]: https://github.com/sol/doctest-haskell/blob/master/README.markdown#usage "doctest Usage Guide"
[install-from-github-script]: ./etc/bolton-install-github.sh "Bolton Github Install Script"

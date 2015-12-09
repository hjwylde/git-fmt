# git-fmt

[![Project Status: Wip - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/1.0.0/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/hjwylde/git-fmt.svg?branch=master)](https://travis-ci.org/hjwylde/git-fmt)
[![Release](https://img.shields.io/github/release/hjwylde/git-fmt.svg)](https://github.com/hjwylde/git-fmt/releases/latest)
[![git-fmt on Stackage LTS](https://www.stackage.org/package/git-fmt/badge/lts)](https://www.stackage.org/lts/package/git-fmt)
[![git-fmt on Stackage Nightly](https://www.stackage.org/package/git-fmt/badge/nightly)](https://www.stackage.org/nightly/package/git-fmt)

Custom git command for formatting code.
git-fmt provides a wrapper around [omnifmt](https://github.com/hjwylde/omnifmt),
    an automatic code formatter.
It adds the ability to operate on specific tracked files in the repository.

Formatted code is:

* Easier to write: never worry about minor formatting concerns while hacking away.
* Easier to read: when all code looks the same you need not mentally convert others' formatting
  style into something you can understand.
* Easier to maintain: mechanical changes to the source don't cause unrelated changes to the file's
  formatting; diffs show only the real changes.
* Uncontroversial: never have a debate about spacing or brace position ever again.

(Bullet points taken from [https://blog.golang.org/go-fmt-your-code](https://blog.golang.org/go-fmt-your-code).)

### Installing

Installing git-fmt is easiest done using either
    [stack](https://github.com/commercialhaskell/stack) (recommended) or
    [Cabal](https://github.com/haskell/cabal).

**Using stack:**

```bash
stack install git-fmt
export PATH=$PATH:~/.local/bin
```

**Using Cabal:**

```bash
cabal-install git-fmt
export PATH=$PATH:~/.cabal/bin
```

### Usage

The git-fmt binary provides an interface for selecting files and piping them through external
    pretty-printers.
It supports both prettifying the files immediately and performing dry-runs to see which files are
    ugly.
Given that it uses the [omnifmt](https://github.com/hjwylde/omnifmt) library underneath, the syntax
    and features are quite similar.
The main difference is that git-fmt restricts files to being tracked by the git repository and that
    by default it only operates on files in the index.

**The basics:**

git-fmt operates only on tracked git files (thus it implicitly respects the '.gitignore' file).
By default it operates on files in the index (i.e., `--operate-on head`).
It is possible to operate on all tracked files (`--operate-on-tracked`) or on a specific reference
    (`--operate-on REF`).
The `REF` argument is passed directly into `git diff REF --name-only`, so you can even play with
    ranges such as `master...`.

Passing arguments to git-fmt will narrow down the operation files.
For example, `git fmt --operate-on-tracked src/` will format all tracked files under 'src/' and
    `git fmt --operate-on head src/` will format all files in the index under 'src/'.

**Modes:**

git-fmt can run in three different modes, *normal*, *dry-run* and *diff*.

The normal and dry-run modes act the same as omnifmt.
Diff mode however uses `git diff` as opposed to `diff`.
By default the diff isn't paged, so to get output similar to `git diff` or `git log` it is
    recommended to use `[-p|--paginate]`, e.g., `git -p fmt -m diff`.

**NB:** it isn't possible to pipe the diff into `git apply` due to the destination file path
    header.

#### Configuration

git-fmt delegates to omnifmt for configuration, see
    [here](https://github.com/hjwylde/omnifmt#configuration) for documentation and examples.


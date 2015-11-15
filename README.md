# git-fmt

[![Project Status: Wip - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/1.0.0/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/hjwylde/git-fmt.svg?branch=master)](https://travis-ci.org/hjwylde/git-fmt)
[![Release](https://img.shields.io/github/release/hjwylde/git-fmt.svg)](https://github.com/hjwylde/git-fmt/releases/latest)

`git-fmt` adds a custom command to Git that automatically formats code.
The idea was taken from [gofmt](https://golang.org/cmd/gofmt/), just with a bit of expansion to more
    languages.

`fmt`'d code is:

* Easier to write: never worry about minor formatting concerns while hacking away.
* Easier to read: when all code looks the same you need not mentally convert others' formatting
  style into something you can understand.
* Easier to maintain: mechanical changes to the source don't cause unrelated changes to the file's
  formatting; diffs show only the real changes.
* Uncontroversial: never have a debate about spacing or brace position ever again.

(Bullet points taken from https://blog.golang.org/go-fmt-your-code)

### Installing

Installing `git-fmt` is easiest done using either
    [stack](https://github.com/commercialhaskell/stack) (recommended) or
    [Cabal](https://github.com/haskell/cabal).
Alternatively you may download a pre-compiled binary of the
    [latest release](https://github.com/hjwylde/git-fmt/releases/latest).

**Using stack:**

```bash
stack install qux
export PATH=$PATH:~/.local/bin
```

**Using Cabal:**

```bash
cabal-install qux
export PATH=$PATH:~/.cabal/bin
```

**Using a release:**

1. Download the appropriate binary for your system from the [latest release](https://github.com/hjwylde/git-fmt/releases/latest).
2. Place the binary somewhere it will be included in your `$PATH`.

### Formatting

The formatter is designed to produce pretty, readable code.
It adheres to a few properties and styling rules to accomplish this.

**Properties:**

The formatter is:
* Portable: it behaves the same across operating systems.
* Consistent: it produces code that behaves exactly the same as before.
* Correct: it produces valid code.
* Idempotent: calling it multiple times results in the same output.

The formatter is not necessarily _complete_ as many parsers implement strange parsing rules that
    can't be replicated easily.
As such, there may be inputs that it is unable to parse;
    in these scenarios it simply outputs a warning and moves on to the next input.

**Styling:**

On top of providing a standard styling to all language elements,
    the formatter performs the following changes:
* Sorts imports and declarations where possible.
* Limits the line length to 100 characters.

Further, it ensures that:
* Comments are maintained.
* Line breaks between statements are maintained (up to a maximum of 1).

### Languages

The following languages are supported:
* JSON

For examples on how a specific language is formatted, see the test examples.


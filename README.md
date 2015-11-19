# git-fmt

[![Project Status: Wip - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/1.0.0/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/hjwylde/git-fmt.svg?branch=master)](https://travis-ci.org/hjwylde/git-fmt)
[![Release](https://img.shields.io/github/release/hjwylde/git-fmt.svg)](https://github.com/hjwylde/git-fmt/releases/latest)

(Side note: the formatting component of this project will eventually be split out and named omnifmt.)

`git-fmt` adds a custom command to Git that automatically formats code by using external
    pretty-printers.
The idea was taken from [gofmt](https://golang.org/cmd/gofmt/), just with a bit of expansion to more
    languages.

Formatted code is:

* Easier to write: never worry about minor formatting concerns while hacking away.
* Easier to read: when all code looks the same you need not mentally convert others' formatting
  style into something you can understand.
* Easier to maintain: mechanical changes to the source don't cause unrelated changes to the file's
  formatting; diffs show only the real changes.
* Uncontroversial: never have a debate about spacing or brace position ever again.

(Bullet points taken from https://blog.golang.org/go-fmt-your-code.)

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

### Usage

`git-fmt` itself just provides a way to select files in the git repository and pipe them through a
    pretty-printer.
It also provides a dry-run mode that will show you which files need prettifying.

#### Configuration

Configuration is done through an `.omnifmt.yaml` file in the repository's top-level directory.

To show how, here's an example `.omnifmt.yaml`:
```yaml
haskell:
    extensions: ["hs", "lhs"]
    command:    "stylish-haskell {{inputFileName}} > {{tmpFileName}}"

javascript:
    extensions: ["js"]
    command:    "js-beautify {{inputFileName}} > {{tmpFileName}}"

json:
    extensions: ["json"]
    command:    "cat {{inputFileName}} | json_pp > {{tmpFileName}}"
```

That's all it takes!
Each command must declare how to read the input file and how to write to the temporary file.
The temporary file is used to compare whether the original input was pretty or ugly before writing
    to the original.

Extensions is pretty self explanatory, but if you use the same extension more than once then
    precedence goes to the first defined pretty-printer.


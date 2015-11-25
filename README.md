# git-fmt

[![Project Status: Wip - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/1.0.0/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/hjwylde/git-fmt.svg?branch=master)](https://travis-ci.org/hjwylde/git-fmt)
[![Release](https://img.shields.io/github/release/hjwylde/git-fmt.svg)](https://github.com/hjwylde/git-fmt/releases/latest)

(Side note: the formatting component of this project will eventually be split out and named omnifmt.)

`git-fmt` was created to make prettifying code easy.
It adds a custom (easy to use) command to Git that formats code through external pretty-printers.

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

The `git-fmt` binary wraps external pretty-printers and provides an interface for selecting files
    to prettify and performing dry-runs.
Rather than repeating it here, have a read of `git fmt -h` for how to call it.

#### Configuration

Configuration is done via an `.omnifmt.yaml` file in the git repository.
The file contains a list of *programs* that link *extensions* to a prettifying *command*, e.g.,
```yaml
haskell:
    extensions: ["hs", "lhs"]
    command:    "stylish-haskell {{input}} > {{output}}"

javascript:
    extensions: ["js"]
    command:    "js-beautify -f {{input}}"

json:
    extensions: ["json"]
    command:    "json_pp"

ruby:
    extensions: ["rb"]
    command:    "ruby-beautify"
```

Each command declares how to read the *input file* and how to write to the *output file*.
If the input variable is omitted, the file contents are fed to the command through `stdin`.
Likewise if the output variable is omitted, the pretty contents are read from `stdout`.
The output file is used to compare whether the original was pretty or ugly before writing to it.

The extensions field is pretty self explanatory, but if you use the same extension more than once
    then precedence goes to the program defined first.

#### Examples

See the [docs/example-configs/](https://github.com/hjwylde/git-fmt/tree/master/docs/example-configs/)
    directory for some common pretty-printers and their corresponding omnifmt config (pull requests
    welcome for more).
Just don't forget to actually call the config file `.omnifmt.yaml`!

**NB:** I haven't tested them fully, be careful in case one is buggy.


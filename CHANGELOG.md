## Changelog

#### Upcoming

#### v0.4.1.0

*Minor*

* Added bash completion for `--mode` and arguments. ([#71](https://github.com/hjwylde/git-fmt/issues/71))

*Revisions*

* Changed path outputs to be relative to the root directory. ([#69](https://github.com/hjwylde/git-fmt/issues/69))
* Fixed a bug where `--operate-on` didn't work in subdirectories. ([#69](https://github.com/hjwylde/git-fmt/issues/69))

#### v0.4.0.0

*Major*

* Extracted omnifmt out to git@github.com:hjwylde/omnifmt. ([#41](https://github.com/hjwylde/git-fmt/issues/41))

#### v0.3.1.2

*Revisions*

* Fixed a bug causing prettifying to fail across filesystem boundaries. ([#72](https://github.com/hjwylde/git-fmt/issues/72))

#### v0.3.1.1

*Revisions*

* Fixed a bug causing the program to hang when not in the root directory. ([#66](https://github.com/hjwylde/git-fmt/issues/66))
* Fixed a bug that omitted searching the drive for a config file. ([#66](https://github.com/hjwylde/git-fmt/issues/66))
* Fixed a bug where output files could be created outside of the temp directory. ([#68](https://github.com/hjwylde/git-fmt/issues/68))

#### v0.3.1.0

*Minor*

* Added timeout wrapper for the program command. ([#52](https://github.com/hjwylde/git-fmt/issues/52))
* Added `diff` mode. ([#23](https://github.com/hjwylde/git-fmt/issues/23))

#### v0.3.0.5

*Revisions*

* Fixed a bug causing prettifying to fail across filesystem boundaries. ([#72](https://github.com/hjwylde/git-fmt/issues/72))

#### v0.3.0.4

*Revisions*

* Fixed a bug causing the program to hang when not in the root directory. ([#66](https://github.com/hjwylde/git-fmt/issues/66))
* Fixed a bug that omitted searching the drive for a config file. ([#66](https://github.com/hjwylde/git-fmt/issues/66))
* Fixed a bug where output files could be created outside of the temp directory. ([#68](https://github.com/hjwylde/git-fmt/issues/68))

#### v0.3.0.3

*Revisions*

* Restricted use of `--operate-on-tracked` and `--operate-on REF` at the same time. ([#65](https://github.com/hjwylde/git-fmt/issues/65))
* Removed long option for help text (as git overrides it for man pages). ([#65](https://github.com/hjwylde/git-fmt/issues/65))

#### v0.3.0.2

*Revisions*

* Fixed a bug where passing arguments didn't properly narrow down the operation files. ([#64](https://github.com/hjwylde/git-fmt/issues/64))

#### v0.3.0.1

*Revisions*

* Relaxed version constraints. ([#63](https://github.com/hjwylde/git-fmt/issues/63))

#### v0.3.0.0

*Major*

* Restricted arguments to being inside the repository. ([#34](https://github.com/hjwylde/git-fmt/issues/34))
* Refactored library to use pipes. ([#32](https://github.com/hjwylde/git-fmt/issues/32))
* Made `Options` and `Version` modules private. ([#62](https://github.com/hjwylde/git-fmt/issues/62))
* Renamed library modules to `Omnifmt`. ([#62](https://github.com/hjwylde/git-fmt/issues/62))
* Refactored pipeline to feed and consume triples. ([#61](https://github.com/hjwylde/git-fmt/issues/61))
* Set default `--operate-on` to `head`. ([#28](https://github.com/hjwylde/git-fmt/issues/28))

*Minor*

* Changed "not found" status to print as debug message. ([#61](https://github.com/hjwylde/git-fmt/issues/61))
* Added "unsupported" status as debug message. ([#61](https://github.com/hjwylde/git-fmt/issues/61))
* Added `--operate-on-tracked` and `--operate-on REF` options. ([#28](https://github.com/hjwylde/git-fmt/issues/28))

#### v0.2.2.1

*Revisions*

* Fixed a bug causing prettifying to fail across filesystem boundaries. ([#72](https://github.com/hjwylde/git-fmt/issues/72))

#### v0.2.2.0

*Minor*

* Added `--threads` option to change the number of threads for parallelisation. ([#54](https://github.com/hjwylde/git-fmt/issues/54))

*Revisions*

* Added quoting to the command variables during substitution. ([#59](https://github.com/hjwylde/git-fmt/issues/59))
* Changed parallelisation to use the number of capabilities (and processors) for the number of
  threads by default. ([#54](https://github.com/hjwylde/git-fmt/issues/54))

#### v0.2.1.2

*Revisions*

* Fixed a bug causing prettifying to fail across filesystem boundaries. ([#72](https://github.com/hjwylde/git-fmt/issues/72))

#### v0.2.1.1

*Revisions*

* Fixed a bug where passing arguments would only work when running in the git directory. ([#57](https://github.com/hjwylde/git-fmt/issues/57))

#### v0.2.1.0

*Minor*

* Added default use of stdin and stdout when variables not specified in a program command. ([#49](https://github.com/hjwylde/git-fmt/issues/49))

#### v0.2.0.2

*Revisions*

* Fixed a bug causing prettifying to fail across filesystem boundaries. ([#72](https://github.com/hjwylde/git-fmt/issues/72))

#### v0.2.0.1

*Revisions*

* Fixed a bug where passing arguments would only work when running in the git directory. ([#57](https://github.com/hjwylde/git-fmt/issues/57))

#### v0.2.0.0

*Major*

* Removed `--list-ugly` and `--dry-run` options. ([#29](https://github.com/hjwylde/git-fmt/issues/29))
* Restricted use of `--quiet` and `--verbose` at the same time. ([#35](https://github.com/hjwylde/git-fmt/issues/35))
* Updated project structure to delegate pretty printing to other binaries. ([#38](https://github.com/hjwylde/git-fmt/issues/38))
* Added a `.omniyaml.yaml` config file. ([#38](https://github.com/hjwylde/git-fmt/issues/38))

*Minor*

* Added `--mode` option (either `normal` or `dry-run`). ([#29](https://github.com/hjwylde/git-fmt/issues/29))
* Added `--null` option (use the null terminator as the delimiter for inputs). ([#27](https://github.com/hjwylde/git-fmt/issues/27))
* Added support for directories as arguments (directories include all files within recursively). ([#30](https://github.com/hjwylde/git-fmt/issues/30))
* Added parallelisation. ([#48](https://github.com/hjwylde/git-fmt/issues/48))

*Revisions*

* Added a warning for when files aren't found. ([#29](https://github.com/hjwylde/git-fmt/issues/29))
* Updated internal use of `git ls-files` to use the null terminator option. ([#27](https://github.com/hjwylde/git-fmt/issues/27))
* Fixed debug log messages to have timestamp and log level on all lines. ([#33](https://github.com/hjwylde/git-fmt/issues/33))
* Tidied up error messages from git. ([#40](https://github.com/hjwylde/git-fmt/issues/40))
* Tidied up error messages from parsing the config. ([#43](https://github.com/hjwylde/git-fmt/issues/43))

#### v0.1.0.3

*Revisions*

* Fixed a bug where passing arguments would only work when running in the git directory. ([#57](https://github.com/hjwylde/git-fmt/issues/57))

#### v0.1.0.2

*Revisions*

* Fixed a bug where UTF-8 characters in strings weren't printed properly. ([#26](https://github.com/hjwylde/git-fmt/issues/26))

#### v0.1.0.1

*Revisions*

* Fixed a bug where integers were printed as rationals. ([#25](https://github.com/hjwylde/git-fmt/issues/25))

#### v0.1.0.0

This is first release of the `git-fmt` binary!
It provides a basic syntax for formatting files in a git repository.
Currently only JSON is supported.


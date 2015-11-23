## Changelog

#### Upcoming

*Minor*
* Added `--threads` option to change the number of threads for parallelisation. ([#54](https://github.com/hjwylde/git-fmt/issues/54))

*Revisions*
* Added quoting to the command variables during substitution. ([#59](https://github.com/hjwylde/git-fmt/issues/59))
* Changed parallelisation to use the number of capabilities (and processors) for the number of
  threads by default. ([#54](https://github.com/hjwylde/git-fmt/issues/54))

#### v0.2.1.1

*Revisions*
* Fixed a bug where passing arguments would only work when running in the git directory. ([#57](https://github.com/hjwylde/git-fmt/issues/57))

#### v0.2.1.0

*Minor*
* Added default use of stdin and stdout when variables not specified in a program command. ([#49](https://github.com/hjwylde/git-fmt/issues/49))

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


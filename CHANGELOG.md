## Changelog

#### Upcoming in master

*Major*
* Removed `--list-ugly` and `--dry-run` options. ([#29](https://github.com/hjwylde/git-fmt/issues/29))

*Minor*
* Added `--mode` option (either `normal` or `dry-run`). ([#29](https://github.com/hjwylde/git-fmt/issues/29))
* Added `--null` option (use the null terminator as the delimiter for inputs). ([#27](https://github.com/hjwylde/git-fmt/issues/27))
* Added support for directories as arguments (directories include all files within recursively).

*Revisions*
* Added a warning for when files aren't found. ([#29](https://github.com/hjwylde/git-fmt/issues/29))
* Updated internal use of `git ls-files` to use the null terminator option. ([#27](https://github.com/hjwylde/git-fmt/issues/27))

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


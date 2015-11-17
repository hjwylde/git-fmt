## Changelog

#### Upcoming in master

*Major*
* Added `--mode` option (includes the now removed `--list-ugly` and `--dry-run`). ([#29](https://github.com/hjwylde/git-fmt/issues/29))

*Revisions*
* Added a warning for when files aren't found. ([#29](https://github.com/hjwylde/git-fmt/issues/29))

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


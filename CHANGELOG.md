## Changelog

#### Upcoming in master

*Minor*
* Added a null terminator option (`-0` or `--null`). ([#27](https://github.com/hjwylde/git-fmt/issues/27))

*Revisions*
* Updated internal use of `git ls-files` to use the null terminator option. ([#27](https://github.com/hjwylde/git-fmt/issues/27))

#### v0.1.0.2

* Fixed a bug where UTF-8 characters in strings weren't printed properly.

#### v0.1.0.1

* Fixed a bug where integers were printed as rationals.

#### v0.1.0.0

This is first release of the `git-fmt` binary!
It provides a basic syntax for formatting files in a git repository.
Currently only JSON is supported.


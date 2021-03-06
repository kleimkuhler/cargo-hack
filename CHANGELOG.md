# Changelog

All notable changes to this project will be documented in this file.

This project adheres to [Semantic Versioning](https://semver.org).

## [Unreleased]

## [0.3.2] - 2019-12-09

* [Added `--feature-powerset` flag to perform for the feature powerset.][23]

* [Reduced compile time of `cargo-hack` to less than half.][22]

[22]: https://github.com/taiki-e/cargo-hack/pull/22
[23]: https://github.com/taiki-e/cargo-hack/pull/23

## [0.3.1] - 2019-11-20

* [cargo-hack can now handle ctrl-c signal properly.][20] Previously there was an issue with interoperability with `--no-dev-deps` flag.

[20]: https://github.com/taiki-e/cargo-hack/pull/20

## [0.3.0] - 2019-11-13

* [cargo-hack now works on windows.][17]

* [Fixed an issue that when `--all`(`--workspace`) and `--package` flags are run in subcrate, the command does not apply to other crates in the workspace.][17]

* [Banned `--no-dev-deps` flag with builds that require dev-dependencies.][16]

* [cargo-hack is no longer does not generate temporary backup files.][14]

[14]: https://github.com/taiki-e/cargo-hack/pull/14
[16]: https://github.com/taiki-e/cargo-hack/pull/16
[17]: https://github.com/taiki-e/cargo-hack/pull/17

## [0.2.1] - 2019-11-03

* Removed warning from `--all`/`--workspace` flag. This is no longer "experimental".

## [0.2.0] - 2019-11-02

* [Implemented `--package` flag.][12]

* [Implemented `--exclude` flag.][12]

* [Renamed `--ignore-non-exist-features` flag to `--ignore-unknown-features`.][10]
  The old name can be used as an alias, but is deprecated.

[10]: https://github.com/taiki-e/cargo-hack/pull/10
[12]: https://github.com/taiki-e/cargo-hack/pull/12

## [0.1.1] - 2019-11-01

* Fixed some issues on Windows.

## [0.1.0] - 2019-10-30

Initial release

[Unreleased]: https://github.com/taiki-e/cargo-hack/compare/v0.3.2...HEAD
[0.3.2]: https://github.com/taiki-e/cargo-hack/compare/v0.3.1...v0.3.2
[0.3.1]: https://github.com/taiki-e/cargo-hack/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/taiki-e/cargo-hack/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/taiki-e/cargo-hack/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/taiki-e/cargo-hack/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/taiki-e/cargo-hack/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/taiki-e/cargo-hack/releases/tag/v0.1.0

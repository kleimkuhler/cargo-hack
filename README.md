# cargo-hack

[![crates-badge]][crates-url]
[![license-badge]][license]
[![rustc-badge]][rustc-url]

[crates-badge]: https://img.shields.io/crates/v/cargo-hack.svg
[crates-url]: https://crates.io/crates/cargo-hack
[license-badge]: https://img.shields.io/crates/l/cargo-hack.svg
[license]: #license
[rustc-badge]: https://img.shields.io/badge/rustc-1.36+-lightgray.svg
[rustc-url]: https://blog.rust-lang.org/2019/07/04/Rust-1.36.0.html

A tool to work around some limitations on cargo.

Cargo is a great tool but has some limitations.
This tool provides additional flags to avoid some of these limitations.

## Installation

```sh
cargo install cargo-hack
```

To install the current cargo-hack requires Rust 1.36 or later.

## Usage

`cargo-hack` is basically wrapper of `cargo` that propagates subcommand and most of the passed flags to `cargo`, but provides additional flags and changes the behavior of some existing flags.

* **`--each-feature`**

  Perform for each feature which includes default features and `--no-default-features` of the package.

  This is useful to check that each feature is working properly. (When used for this purpose, it is recommended to use with `--no-dev-deps` to avoid [cargo#4866].)

  ```sh
  cargo hack check --each-feature --no-dev-deps
  ```

  This is a workaround for an issue that cargo does not support for `--features` and `--no-default-features` flags for sub crates ([cargo#3620], [cargo#4106], [cargo#4463], [cargo#4753], [cargo#5015], [cargo#5364], [cargo#6195]).

* **`--feature-powerset`**

  Perform for the feature powerset which includes `--no-default-features` and
  default features of the package.

  This is useful to check that every combination of features is working
  properly. (When used for this purpose, it is recommended to use with
  `--no-dev-deps` to avoid [cargo#4866].)

  ```sh
  cargo hack check --feature-powerset --no-dev-deps
  ```

* **`--no-dev-deps`**

  Perform without dev-dependencies.

  This is a workaround for an issue that dev-dependencies leaking into normal build ([cargo#4866]).

  Also, this can be used as a workaround for an issue that `cargo` does not allow publishing a package with cyclic dev-dependencies. ([cargo#4242])

  ```sh
  cargo hack publish --no-dev-deps --dry-run --allow-dirty
  ```

  Currently, using `--no-dev-deps` flag removes dev-dependencies from real manifest while cargo-hack is running and restores it when finished. See [cargo#4242] for why this is necessary.
  Also, this behavior may change in the future on some subcommands. See also [#15].

* **`--remove-dev-deps`**

  Equivalent to `--no-dev-deps` except for does not restore the original `Cargo.toml` after execution.

  This is useful to know what Cargo.toml that cargo-hack is actually using with `--no-dev-deps`.

  *This flag also works without subcommands.*

* **`--ignore-private`**

  Skip to perform on `publish = false` packages.

* **`--ignore-unknown-features`**

  Skip passing `--features` to `cargo` if that feature does not exist.

  This is a workaround for an issue that `cargo` does not support for `--features` with workspace ([cargo#3620], [cargo#4106], [cargo#4463], [cargo#4753], [cargo#5015], [cargo#5364], [cargo#6195]).

  This feature was formerly called `--ignore-non-exist-features`, but has been renamed. The old name can be used as an alias, but is deprecated.

`cargo-hack` changes the behavior of the following existing flags.

* **`--features`**, **`--no-default-features`**

  Unlike `cargo` ([cargo#3620], [cargo#4106], [cargo#4463], [cargo#4753], [cargo#5015], [cargo#5364], [cargo#6195]), it can also be applied to sub-crates.

* **`--all`**, **`--workspace`**

  Perform command for all packages in the workspace.

  For example, running `cargo hack check --all` in a workspace with members `foo` and `bar` behaves almost the same as the following script:

  ```sh
  # If you use cargo-hack, you don't need to maintain this list manually.
  members=("foo" "bar")

  for member in ${members[@]}; do
      cargo check --manifest-path "${member}"
  done
  ```

  Workspace members will be performed according to the order of the 'packages' fields of [cargo-metadata].

[#3]: https://github.com/taiki-e/cargo-hack/issues/3
[#15]: https://github.com/taiki-e/cargo-hack/issues/15
[cargo#3620]: https://github.com/rust-lang/cargo/issues/3620
[cargo#4106]: https://github.com/rust-lang/cargo/issues/4106
[cargo#4463]: https://github.com/rust-lang/cargo/issues/4463
[cargo#4753]: https://github.com/rust-lang/cargo/issues/4753
[cargo#4866]: https://github.com/rust-lang/cargo/issues/4866
[cargo#5015]: https://github.com/rust-lang/cargo/issues/4463
[cargo#5364]: https://github.com/rust-lang/cargo/issues/5364
[cargo#6195]: https://github.com/rust-lang/cargo/issues/6195
[cargo#4242]: https://github.com/rust-lang/cargo/issues/4242
[cargo-metadata]: https://doc.rust-lang.org/cargo/commands/cargo-metadata.html

## License

Licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
* MIT license ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.

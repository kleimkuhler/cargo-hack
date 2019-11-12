use std::{
    borrow::Cow,
    collections::BTreeMap,
    env,
    ffi::OsString,
    io::{self, Write},
    path::PathBuf,
    process::Command,
};

use anyhow::{Context, Result};
// use serde::Deserialize;

use crate::Args;

// Refs:
// * https://github.com/rust-lang/cargo/blob/0.40.0/src/cargo/ops/cargo_output_metadata.rs#L79-L86
// * https://github.com/rust-lang/cargo/blob/0.40.0/src/cargo/core/package.rs#L57-L80
// * https://github.com/oli-obk/cargo_metadata

#[derive(Debug)]
// #[derive(Debug, Deserialize)]
pub(crate) struct Metadata {
    /// A list of all crates referenced by this crate (and the crate itself)
    pub(crate) packages: Vec<Package>,
    // /// A list of all workspace members
    // pub(crate) workspace_members: Vec<PackageId>,
    // /// Dependencies graph
    // pub(crate) resolve: Option<MetadataResolve>,
    // /// Build directory
    // pub(crate) target_directory: PathBuf,
    // version: u32,
    /// Workspace root
    pub(crate) workspace_root: PathBuf,
}

#[derive(Debug)]
// #[derive(Debug, Deserialize)]
pub(crate) struct Package {
    /// Name as given in the `Cargo.toml`
    pub(crate) name: String,
    // /// Version given in the `Cargo.toml`
    // pub(crate) version: String,
    // /// An opaque identifier for a package
    // pub(crate) id: PackageId,
    // /// License as given in the `Cargo.toml`
    // pub(crate) license: Option<String>,
    // /// If the package is using a nonstandard license, this key may be specified instead of
    // /// `license`, and must point to a file relative to the manifest.
    // pub(crate) license_file: Option<String>,
    // pub(crate) description: Option<String>,
    // /// The source of the package, e.g.
    // /// crates.io or `None` for local projects.
    // pub(crate) source: SourceId,
    // /// Description as given in the `Cargo.toml`
    // pub(crate) dependencies: Vec<Dependency>,
    // /// Targets provided by the crate (lib, bin, example, test, ...)
    // pub(crate) targets: Vec<Target>,
    /// Features provided by the crate, mapped to the features required by that feature.
    pub(crate) features: BTreeMap<String, Vec<String>>,
    /// Path containing the `Cargo.toml`
    pub(crate) manifest_path: PathBuf,
    // /// Contents of the free form package.metadata section
    // pub(crate) metadata: Option<Value>,
    // /// List of registries to which this package may be published.
    // ///
    // /// Publishing is unrestricted if `None`, and forbidden if the `Vec` is empty.
    // ///
    // /// This is always `None` if running with a version of Cargo older than 1.39.
    // pub(crate) publish: Option<Vec<String>>,
    // /// Authors given in the `Cargo.toml`
    // pub(crate) authors: Vec<String>,
    // /// Categories as given in the `Cargo.toml`
    // pub(crate) categories: Vec<String>,
    // /// Keywords as given in the `Cargo.toml`
    // pub(crate) keywords: Vec<String>,
    // /// Readme as given in the `Cargo.toml`
    // pub(crate) readme: Option<String>,
    // /// Repository as given in the `Cargo.toml`
    // pub(crate) repository: Option<String>,
    // /// Default Rust edition for the package
    // pub(crate) edition: String,
    // /// The name of a native library the package is linking to.
    // pub(crate) links: Option<String>,
    // pub(crate) metabuild: Option<Vec<String>>, // unstable: https://github.com/rust-lang/rust/issues/49803
}

impl Metadata {
    pub(crate) fn new(args: &Args) -> Result<Self> {
        let cargo = env::var_os("CARGO").unwrap_or_else(|| OsString::from("cargo"));
        let mut command = Command::new(cargo);
        command.args(&["metadata", "--no-deps", "--format-version=1"]);
        if let Some(manifest_path) = &args.manifest_path {
            command.arg("--manifest-path");
            command.arg(manifest_path);
        }

        let output = command.output().context("failed to run 'cargo metadata'")?;
        if !output.status.success() {
            let _ = io::stderr().write_all(&output.stderr);
            let code = output.status.code().unwrap_or(1);
            std::process::exit(code);
        }

        serde_json::from_slice(&output.stdout).context("failed to parse metadata")
    }
}

impl Package {
    pub(crate) fn name_verbose(&self, args: &Args) -> Cow<'_, str> {
        if args.verbose {
            Cow::Owned(format!(
                "{} ({})",
                self.name,
                self.manifest_path.parent().unwrap().display()
            ))
        } else {
            Cow::Borrowed(&self.name)
        }
    }
}

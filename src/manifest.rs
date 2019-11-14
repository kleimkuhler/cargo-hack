use std::{
    collections::BTreeMap,
    fs,
    ops::Deref,
    path::{Path, PathBuf},
};

use anyhow::{bail, Context, Result};

use crate::{
    metadata,
    toml::{Publish, TomlManifest, TomlProject, TomlWorkspace},
};

// Based on https://github.com/rust-lang/cargo/blob/0.39.0/src/cargo/util/important_paths.rs
/// Finds the root `Cargo.toml`.
pub(crate) fn find_root_manifest_for_wd(cwd: &Path) -> Result<PathBuf> {
    for current in cwd.ancestors() {
        let manifest = current.join("Cargo.toml");
        if manifest.exists() {
            return Ok(manifest);
        }
    }

    bail!("could not find `Cargo.toml` in `{}` or any parent directory", cwd.display())
}

#[derive(Debug)]
pub(crate) struct Manifest {
    pub(crate) path: PathBuf,
    pub(crate) raw: String,
    toml: TomlManifest,
}

impl Manifest {
    pub(crate) fn new(path: impl Into<PathBuf>) -> Result<Self> {
        let path = path.into();
        let raw = fs::read_to_string(&path)
            .with_context(|| format!("failed to read manifest from {}", path.display()))?;
        let toml = toml::from_str(&raw)
            .with_context(|| format!("failed to parse manifest file: {}", path.display()))?;
        Ok(Self { path, raw, toml })
    }

    pub(crate) fn package_name(&self) -> &str {
        assert!(!self.is_virtual());
        &self.package.as_ref().unwrap().name
    }

    pub(crate) fn is_virtual(&self) -> bool {
        self.package.is_none()
    }

    // `metadata.package.publish` requires Rust 1.39
    pub(crate) fn is_private(&self) -> bool {
        assert!(!self.is_virtual());
        self.package.as_ref().unwrap().publish == false
    }

    pub(crate) fn remove_dev_deps(&self) -> Result<String> {
        Ok(super::remove_dev_deps::remove_dev_deps(&self.raw))
    }
}

impl Deref for Manifest {
    type Target = TomlManifest;

    fn deref(&self) -> &Self::Target {
        &self.toml
    }
}

fn make_project(package: &metadata::Package, current_dir: &Path) -> (TomlManifest, PathBuf) {
    let manifest = TomlManifest {
        package: Some(TomlProject {
            edition: Some("2018".into()),
            name: "check_".to_string() + &package.name,
            version: "0.0.0".to_string(),
            publish: Publish::Flag(false),
        }),
        lib: None,
        dependencies: Some(BTreeMap::new()),
        workspace: Some(TomlWorkspace { members: None }),
    };

    unimplemented!()
}

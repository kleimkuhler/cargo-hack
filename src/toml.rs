use std::{collections::BTreeMap, path::PathBuf};

// use serde::{Deserialize, Serialize};

// Refs:
// * https://github.com/rust-lang/cargo/blob/0.40.0/src/cargo/util/toml/mod.rs
// * https://gitlab.com/crates.rs/cargo_toml

// #[derive(Debug, Deserialize, Serialize)]
#[derive(Debug)]
// #[serde(rename_all = "kebab-case")]
pub(crate) struct TomlManifest {
    // cargo_features: Option<Vec<String>>, // unstable
    pub(crate) package: Option<TomlProject>,
    // pub(crate) project: Option<TomlProject>, // TODO?
    // profile: Option<TomlProfiles>,
    // Only used for serializing because metadata has more detailed information about this.
    // #[serde(skip_deserializing)]
    pub(crate) lib: Option<TomlTarget>,
    // pub(crate) bin: Option<Vec<TomlTarget>>,
    // pub(crate) example: Option<Vec<TomlTarget>>,
    // pub(crate) test: Option<Vec<TomlTarget>>,
    // pub(crate) bench: Option<Vec<TomlTarget>>,
    // Only used for serializing because metadata has more detailed information about this.
    // #[serde(skip_deserializing)]
    pub(crate) dependencies: Option<BTreeMap<String, TomlDependency>>,
    // dev_dependencies: Option<BTreeMap<String, TomlDependency>>,
    // #[serde(rename = "dev_dependencies")]
    // dev_dependencies2: Option<BTreeMap<String, TomlDependency>>,
    // build_dependencies: Option<BTreeMap<String, TomlDependency>>,
    // #[serde(rename = "build_dependencies")]
    // build_dependencies2: Option<BTreeMap<String, TomlDependency>>,
    // Metadata has more detailed information about this.
    // features: Option<BTreeMap<String, Vec<String>>>,
    // Metadata has more detailed information about this.
    // target: Option<BTreeMap<String, TomlPlatform>>,
    // replace: Option<BTreeMap<String, TomlDependency>>, // TODO?
    // patch: Option<BTreeMap<String, BTreeMap<String, TomlDependency>>>, // TODO?
    pub(crate) workspace: Option<TomlWorkspace>,
    // badges: Option<BTreeMap<String, BTreeMap<String, String>>>,
}

#[derive(Debug)]
// #[derive(Debug, Deserialize, Serialize)]
// #[serde(rename_all = "kebab-case")]
pub(crate) struct TomlWorkspace {
    pub(crate) members: Option<Vec<String>>,
    // pub(crate) default_members: Option<Vec<String>>,
    // pub(crate) exclude: Option<Vec<String>>,
}

/// Corresponds to a `target` entry, but `TomlTarget` is already used.
#[derive(Debug)]
// #[derive(Debug, Deserialize, Serialize)]
// #[serde(rename_all = "kebab-case")]
pub(crate) struct TomlPlatform {
    pub(crate) dependencies: Option<BTreeMap<String, TomlDependency>>,
    // pub(crate) build_dependencies: Option<BTreeMap<String, TomlDependency>>,
    // #[serde(rename = "build_dependencies")]
    // pub(crate) build_dependencies2: Option<BTreeMap<String, TomlDependency>>,
    // pub(crate) dev_dependencies: Option<BTreeMap<String, TomlDependency>>,
    // #[serde(rename = "dev_dependencies")]
    // pub(crate) dev_dependencies2: Option<BTreeMap<String, TomlDependency>>,
}

#[derive(Debug, Default)]
// #[derive(Debug, Default, Deserialize, Serialize)]
// #[serde(rename_all = "kebab-case")]
pub(crate) struct TomlTarget {
    pub(crate) name: Option<String>,
    pub(crate) crate_type: Option<Vec<String>>,
    // #[serde(rename = "crate_type")]
    // pub(crate) crate_type2: Option<Vec<String>>,
    pub(crate) path: Option<PathBuf>,
    pub(crate) test: Option<bool>,
    pub(crate) doctest: Option<bool>,
    pub(crate) bench: Option<bool>,
    pub(crate) doc: Option<bool>,
    pub(crate) plugin: Option<bool>,
    pub(crate) proc_macro: Option<bool>,
    // #[serde(rename = "proc_macro")]
    // pub(crate) proc_macro2: Option<bool>,
    pub(crate) harness: Option<bool>,
    pub(crate) required_features: Option<Vec<String>>,
    pub(crate) edition: Option<String>,
}

#[derive(Debug)]
// #[derive(Debug, Serialize, Deserialize)]
// #[serde(untagged)]
pub(crate) enum TomlDependency {
    Simple(String),
    Detailed(DetailedTomlDependency),
}

#[derive(Debug, Default)]
// #[derive(Debug, Default, Serialize, Deserialize)]
// #[serde(rename_all = "kebab-case")]
pub(crate) struct DetailedTomlDependency {
    pub(crate) version: Option<String>,
    // pub(crate) registry: Option<String>,
    // /// The URL of the `registry` field.
    // /// This is an internal implementation detail. When Cargo creates a
    // /// package, it replaces `registry` with `registry-index` so that the
    // /// manifest contains the correct URL. All users won't have the same
    // /// registry names configured, so Cargo can't rely on just the name for
    // /// crates published by other users.
    // pub(crate) registry_index: Option<String>,
    pub(crate) path: Option<String>,
    // pub(crate) git: Option<String>,
    // pub(crate) branch: Option<String>,
    // pub(crate) tag: Option<String>,
    // pub(crate) rev: Option<String>,
    pub(crate) features: Option<Vec<String>>,
    pub(crate) optional: Option<bool>,
    pub(crate) default_features: Option<bool>,
    // #[serde(rename = "default_features")]
    // pub(crate) default_features2: Option<bool>,
    // pub(crate) package: Option<String>,
    // pub(crate) public: Option<bool>, // unstable https://github.com/rust-lang/rust/issues/44663
}

/// Represents the `package`/`project` sections of a `Cargo.toml`.
///
/// Note that the order of the fields matters, since this is the order they
/// are serialized to a TOML file. For example, you cannot have values after
/// the field `metadata`, since it is a table and values cannot appear after
/// tables.
#[derive(Debug)]
// #[derive(Debug, Deserialize, Serialize)]
// #[serde(rename_all = "kebab-case")]
pub(crate) struct TomlProject {
    pub(crate) edition: Option<String>,
    pub(crate) name: String,
    pub(crate) version: String,
    // pub(crate) authors: Option<Vec<String>>,
    // pub(crate) build: Option<Value>,
    // pub(crate) metabuild: Option<Value>, // unstable https://github.com/rust-lang/rust/issues/49803
    // pub(crate) links: Option<String>,
    // pub(crate) exclude: Option<Vec<String>>,
    // pub(crate) include: Option<Vec<String>>,
    // #[serde(default)]
    pub(crate) publish: Publish,
    // pub(crate) publish_lockfile: Option<bool>, // TODO?
    // pub(crate) workspace: Option<String>,
    // pub(crate) autobins: Option<bool>,
    // pub(crate) autoexamples: Option<bool>,
    // pub(crate) autotests: Option<bool>,
    // pub(crate) autobenches: Option<bool>,
    // pub(crate) namespaced_features: Option<bool>, // unstable https://github.com/rust-lang/cargo/issues/5565
    // pub(crate) default_run: Option<String>,

    // Package metadata.
    // pub(crate) description: Option<String>,
    // pub(crate) homepage: Option<String>,
    // pub(crate) documentation: Option<String>,
    // pub(crate) readme: Option<String>,
    // pub(crate) keywords: Option<Vec<String>>,
    // pub(crate) categories: Option<Vec<String>>,
    // pub(crate) license: Option<String>,
    // pub(crate) license_file: Option<String>,
    // pub(crate) repository: Option<String>,
    // pub(crate) metadata: Option<Value>,
}

#[derive(Debug)]
// #[derive(Debug, Deserialize, Serialize)]
// #[serde(untagged)]
pub(crate) enum Publish {
    Flag(bool),
    Registry(Vec<String>),
}

impl Default for Publish {
    fn default() -> Self {
        Publish::Flag(true)
    }
}

impl PartialEq<Publish> for bool {
    fn eq(&self, p: &Publish) -> bool {
        match p {
            Publish::Flag(flag) => *flag == *self,
            Publish::Registry(reg) => !reg.is_empty() == *self,
        }
    }
}

impl PartialEq<bool> for Publish {
    fn eq(&self, b: &bool) -> bool {
        b.eq(self)
    }
}

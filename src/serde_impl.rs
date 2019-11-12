mod metadata {
    use crate::metadata::*;
    use std::{collections::BTreeMap, fmt, marker::PhantomData, path::PathBuf};
    extern crate serde as _serde;

    impl<'de> _serde::Deserialize<'de> for Metadata {
        fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                __field0,
                __field1,
                __field2,
                __ignore,
            }
            struct __FieldVisitor;
            impl _serde::de::Visitor<'_> for __FieldVisitor {
                type Value = __Field;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "field identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => Ok(__Field::__field0),
                        1u64 => Ok(__Field::__field1),
                        2u64 => Ok(__Field::__field2),
                        _ => Err(_serde::de::Error::invalid_value(
                            _serde::de::Unexpected::Unsigned(__value),
                            &"field index 0 <= i < 3",
                        )),
                    }
                }
                fn visit_str<__E>(self, __value: &str) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "packages" => Ok(__Field::__field0),
                        "target_directory" => Ok(__Field::__field1),
                        "workspace_root" => Ok(__Field::__field2),
                        _ => Ok(__Field::__ignore),
                    }
                }
                fn visit_bytes<__E>(self, __value: &[u8]) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"packages" => Ok(__Field::__field0),
                        b"target_directory" => Ok(__Field::__field1),
                        b"workspace_root" => Ok(__Field::__field2),
                        _ => Ok(__Field::__ignore),
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            struct __Visitor<'de> {
                marker: PhantomData<Metadata>,
                lifetime: PhantomData<&'de ()>,
            }
            impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                type Value = Metadata;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "struct Metadata")
                }
                #[inline]
                fn visit_seq<__A>(self, mut __seq: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::SeqAccess<'de>,
                {
                    let __field0 =
                        match _serde::de::SeqAccess::next_element::<Vec<Package>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    0usize,
                                    &"struct Metadata with 3 elements",
                                ));
                            }
                        };
                    let __field1 = match _serde::de::SeqAccess::next_element::<PathBuf>(&mut __seq)?
                    {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                1usize,
                                &"struct Metadata with 3 elements",
                            ));
                        }
                    };
                    let __field2 = match _serde::de::SeqAccess::next_element::<PathBuf>(&mut __seq)?
                    {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                2usize,
                                &"struct Metadata with 3 elements",
                            ));
                        }
                    };
                    Ok(Metadata {
                        packages: __field0,
                        target_directory: __field1,
                        workspace_root: __field2,
                    })
                }
                #[inline]
                fn visit_map<__A>(self, mut __map: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut __field0: Option<Vec<Package>> = None;
                    let mut __field1: Option<PathBuf> = None;
                    let mut __field2: Option<PathBuf> = None;
                    while let Some(__key) = _serde::de::MapAccess::next_key::<__Field>(&mut __map)?
                    {
                        match __key {
                            __Field::__field0 => {
                                if Option::is_some(&__field0) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "packages",
                                        ),
                                    );
                                }
                                __field0 = Some(_serde::de::MapAccess::next_value::<Vec<Package>>(
                                    &mut __map,
                                )?);
                            }
                            __Field::__field1 => {
                                if Option::is_some(&__field1) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "target_directory",
                                        ),
                                    );
                                }
                                __field1 =
                                    Some(_serde::de::MapAccess::next_value::<PathBuf>(&mut __map)?);
                            }
                            __Field::__field2 => {
                                if Option::is_some(&__field2) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "workspace_root",
                                        ),
                                    );
                                }
                                __field2 =
                                    Some(_serde::de::MapAccess::next_value::<PathBuf>(&mut __map)?);
                            }
                            _ => {
                                let _ = _serde::de::MapAccess::next_value::<_serde::de::IgnoredAny>(
                                    &mut __map,
                                )?;
                            }
                        }
                    }
                    let __field0 = match __field0 {
                        Some(__field0) => __field0,
                        None => _serde::private::de::missing_field("packages")?,
                    };
                    let __field1 = match __field1 {
                        Some(__field1) => __field1,
                        None => _serde::private::de::missing_field("target_directory")?,
                    };
                    let __field2 = match __field2 {
                        Some(__field2) => __field2,
                        None => _serde::private::de::missing_field("workspace_root")?,
                    };
                    Ok(Metadata {
                        packages: __field0,
                        target_directory: __field1,
                        workspace_root: __field2,
                    })
                }
            }
            const FIELDS: &[&str] = &["packages", "target_directory", "workspace_root"];
            _serde::Deserializer::deserialize_struct(
                __deserializer,
                "Metadata",
                FIELDS,
                __Visitor { marker: PhantomData::<Metadata>, lifetime: PhantomData },
            )
        }
    }
    impl<'de> _serde::Deserialize<'de> for Package {
        fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                __field0,
                __field1,
                __field2,
                __ignore,
            }
            struct __FieldVisitor;
            impl _serde::de::Visitor<'_> for __FieldVisitor {
                type Value = __Field;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "field identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => Ok(__Field::__field0),
                        1u64 => Ok(__Field::__field1),
                        2u64 => Ok(__Field::__field2),
                        _ => Err(_serde::de::Error::invalid_value(
                            _serde::de::Unexpected::Unsigned(__value),
                            &"field index 0 <= i < 3",
                        )),
                    }
                }
                fn visit_str<__E>(self, __value: &str) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "name" => Ok(__Field::__field0),
                        "features" => Ok(__Field::__field1),
                        "manifest_path" => Ok(__Field::__field2),
                        _ => Ok(__Field::__ignore),
                    }
                }
                fn visit_bytes<__E>(self, __value: &[u8]) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"name" => Ok(__Field::__field0),
                        b"features" => Ok(__Field::__field1),
                        b"manifest_path" => Ok(__Field::__field2),
                        _ => Ok(__Field::__ignore),
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            struct __Visitor<'de> {
                marker: PhantomData<Package>,
                lifetime: PhantomData<&'de ()>,
            }
            impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                type Value = Package;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "struct Package")
                }
                #[inline]
                fn visit_seq<__A>(self, mut __seq: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::SeqAccess<'de>,
                {
                    let __field0 = match _serde::de::SeqAccess::next_element::<String>(&mut __seq)?
                    {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                0usize,
                                &"struct Package with 3 elements",
                            ));
                        }
                    };
                    let __field1 = match _serde::de::SeqAccess::next_element::<
                        BTreeMap<String, Vec<String>>,
                    >(&mut __seq)?
                    {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                1usize,
                                &"struct Package with 3 elements",
                            ));
                        }
                    };
                    let __field2 = match _serde::de::SeqAccess::next_element::<PathBuf>(&mut __seq)?
                    {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                2usize,
                                &"struct Package with 3 elements",
                            ));
                        }
                    };
                    Ok(Package { name: __field0, features: __field1, manifest_path: __field2 })
                }
                #[inline]
                fn visit_map<__A>(self, mut __map: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut __field0: Option<String> = None;
                    let mut __field1: Option<BTreeMap<String, Vec<String>>> = None;
                    let mut __field2: Option<PathBuf> = None;
                    while let Some(__key) = _serde::de::MapAccess::next_key::<__Field>(&mut __map)?
                    {
                        match __key {
                            __Field::__field0 => {
                                if Option::is_some(&__field0) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("name"),
                                    );
                                }
                                __field0 =
                                    Some(_serde::de::MapAccess::next_value::<String>(&mut __map)?);
                            }
                            __Field::__field1 => {
                                if Option::is_some(&__field1) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "features",
                                        ),
                                    );
                                }
                                __field1 = Some(_serde::de::MapAccess::next_value::<
                                    BTreeMap<String, Vec<String>>,
                                >(&mut __map)?);
                            }
                            __Field::__field2 => {
                                if Option::is_some(&__field2) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "manifest_path",
                                        ),
                                    );
                                }
                                __field2 =
                                    Some(_serde::de::MapAccess::next_value::<PathBuf>(&mut __map)?);
                            }
                            _ => {
                                let _ = _serde::de::MapAccess::next_value::<_serde::de::IgnoredAny>(
                                    &mut __map,
                                )?;
                            }
                        }
                    }
                    let __field0 = match __field0 {
                        Some(__field0) => __field0,
                        None => _serde::private::de::missing_field("name")?,
                    };
                    let __field1 = match __field1 {
                        Some(__field1) => __field1,
                        None => _serde::private::de::missing_field("features")?,
                    };
                    let __field2 = match __field2 {
                        Some(__field2) => __field2,
                        None => _serde::private::de::missing_field("manifest_path")?,
                    };
                    Ok(Package { name: __field0, features: __field1, manifest_path: __field2 })
                }
            }
            const FIELDS: &[&str] = &["name", "features", "manifest_path"];
            _serde::Deserializer::deserialize_struct(
                __deserializer,
                "Package",
                FIELDS,
                __Visitor { marker: PhantomData::<Package>, lifetime: PhantomData },
            )
        }
    }
}

mod toml {
    extern crate serde as _serde;
    use crate::toml::*;
    use std::{collections::BTreeMap, fmt, marker::PhantomData, path::PathBuf};
    impl<'de> _serde::Deserialize<'de> for TomlManifest {
        fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                __field0,
                __field3,
                __ignore,
            }
            struct __FieldVisitor;
            impl _serde::de::Visitor<'_> for __FieldVisitor {
                type Value = __Field;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "field identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => Ok(__Field::__field0),
                        1u64 => Ok(__Field::__field3),
                        _ => Err(_serde::de::Error::invalid_value(
                            _serde::de::Unexpected::Unsigned(__value),
                            &"field index 0 <= i < 2",
                        )),
                    }
                }
                fn visit_str<__E>(self, __value: &str) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "package" => Ok(__Field::__field0),
                        "workspace" => Ok(__Field::__field3),
                        _ => Ok(__Field::__ignore),
                    }
                }
                fn visit_bytes<__E>(self, __value: &[u8]) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"package" => Ok(__Field::__field0),
                        b"workspace" => Ok(__Field::__field3),
                        _ => Ok(__Field::__ignore),
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            struct __Visitor<'de> {
                marker: PhantomData<TomlManifest>,
                lifetime: PhantomData<&'de ()>,
            }
            impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                type Value = TomlManifest;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "struct TomlManifest")
                }
                #[inline]
                fn visit_seq<__A>(self, mut __seq: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::SeqAccess<'de>,
                {
                    let __field0 = match _serde::de::SeqAccess::next_element::<Option<TomlProject>>(
                        &mut __seq,
                    )? {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                0usize,
                                &"struct TomlManifest with 2 elements",
                            ));
                        }
                    };
                    let __field1 = Default::default();
                    let __field2 = Default::default();
                    let __field3 = match _serde::de::SeqAccess::next_element::<Option<TomlWorkspace>>(
                        &mut __seq,
                    )? {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                1usize,
                                &"struct TomlManifest with 2 elements",
                            ));
                        }
                    };
                    Ok(TomlManifest {
                        package: __field0,
                        lib: __field1,
                        dependencies: __field2,
                        workspace: __field3,
                    })
                }
                #[inline]
                fn visit_map<__A>(self, mut __map: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut __field0: Option<Option<TomlProject>> = None;
                    let mut __field3: Option<Option<TomlWorkspace>> = None;
                    while let Some(__key) = _serde::de::MapAccess::next_key::<__Field>(&mut __map)?
                    {
                        match __key {
                            __Field::__field0 => {
                                if Option::is_some(&__field0) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "package",
                                        ),
                                    );
                                }
                                __field0 = Some(_serde::de::MapAccess::next_value::<
                                    Option<TomlProject>,
                                >(&mut __map)?);
                            }
                            __Field::__field3 => {
                                if Option::is_some(&__field3) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "workspace",
                                        ),
                                    );
                                }
                                __field3 = Some(_serde::de::MapAccess::next_value::<
                                    Option<TomlWorkspace>,
                                >(&mut __map)?);
                            }
                            _ => {
                                let _ = _serde::de::MapAccess::next_value::<_serde::de::IgnoredAny>(
                                    &mut __map,
                                )?;
                            }
                        }
                    }
                    let __field0 = match __field0 {
                        Some(__field0) => __field0,
                        None => _serde::private::de::missing_field("package")?,
                    };
                    let __field3 = match __field3 {
                        Some(__field3) => __field3,
                        None => _serde::private::de::missing_field("workspace")?,
                    };
                    Ok(TomlManifest {
                        package: __field0,
                        lib: Default::default(),
                        dependencies: Default::default(),
                        workspace: __field3,
                    })
                }
            }
            const FIELDS: &[&str] = &["package", "workspace"];
            _serde::Deserializer::deserialize_struct(
                __deserializer,
                "TomlManifest",
                FIELDS,
                __Visitor { marker: PhantomData::<TomlManifest>, lifetime: PhantomData },
            )
        }
    }
    impl _serde::Serialize for TomlManifest {
        fn serialize<__S>(&self, __serializer: __S) -> Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            let mut __serde_state = _serde::Serializer::serialize_struct(
                __serializer,
                "TomlManifest",
                false as usize + 1 + 1 + 1 + 1,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "package",
                &self.package,
            )?;
            _serde::ser::SerializeStruct::serialize_field(&mut __serde_state, "lib", &self.lib)?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "dependencies",
                &self.dependencies,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "workspace",
                &self.workspace,
            )?;
            _serde::ser::SerializeStruct::end(__serde_state)
        }
    }
    impl<'de> _serde::Deserialize<'de> for TomlWorkspace {
        fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                __field0,
                __ignore,
            }
            struct __FieldVisitor;
            impl _serde::de::Visitor<'_> for __FieldVisitor {
                type Value = __Field;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "field identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => Ok(__Field::__field0),
                        _ => Err(_serde::de::Error::invalid_value(
                            _serde::de::Unexpected::Unsigned(__value),
                            &"field index 0 <= i < 1",
                        )),
                    }
                }
                fn visit_str<__E>(self, __value: &str) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "members" => Ok(__Field::__field0),
                        _ => Ok(__Field::__ignore),
                    }
                }
                fn visit_bytes<__E>(self, __value: &[u8]) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"members" => Ok(__Field::__field0),
                        _ => Ok(__Field::__ignore),
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            struct __Visitor<'de> {
                marker: PhantomData<TomlWorkspace>,
                lifetime: PhantomData<&'de ()>,
            }
            impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                type Value = TomlWorkspace;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "struct TomlWorkspace")
                }
                #[inline]
                fn visit_seq<__A>(self, mut __seq: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::SeqAccess<'de>,
                {
                    let __field0 = match _serde::de::SeqAccess::next_element::<Option<Vec<String>>>(
                        &mut __seq,
                    )? {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                0usize,
                                &"struct TomlWorkspace with 1 element",
                            ));
                        }
                    };
                    Ok(TomlWorkspace { members: __field0 })
                }
                #[inline]
                fn visit_map<__A>(self, mut __map: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut __field0: Option<Option<Vec<String>>> = None;
                    while let Some(__key) = _serde::de::MapAccess::next_key::<__Field>(&mut __map)?
                    {
                        match __key {
                            __Field::__field0 => {
                                if Option::is_some(&__field0) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "members",
                                        ),
                                    );
                                }
                                __field0 = Some(_serde::de::MapAccess::next_value::<
                                    Option<Vec<String>>,
                                >(&mut __map)?);
                            }
                            _ => {
                                let _ = _serde::de::MapAccess::next_value::<_serde::de::IgnoredAny>(
                                    &mut __map,
                                )?;
                            }
                        }
                    }
                    let __field0 = match __field0 {
                        Some(__field0) => __field0,
                        None => _serde::private::de::missing_field("members")?,
                    };
                    Ok(TomlWorkspace { members: __field0 })
                }
            }
            const FIELDS: &[&str] = &["members"];
            _serde::Deserializer::deserialize_struct(
                __deserializer,
                "TomlWorkspace",
                FIELDS,
                __Visitor { marker: PhantomData::<TomlWorkspace>, lifetime: PhantomData },
            )
        }
    }
    impl _serde::Serialize for TomlWorkspace {
        fn serialize<__S>(&self, __serializer: __S) -> Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            let mut __serde_state = _serde::Serializer::serialize_struct(
                __serializer,
                "TomlWorkspace",
                false as usize + 1,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "members",
                &self.members,
            )?;
            _serde::ser::SerializeStruct::end(__serde_state)
        }
    }
    impl<'de> _serde::Deserialize<'de> for TomlPlatform {
        fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                __field0,
                __ignore,
            }
            struct __FieldVisitor;
            impl _serde::de::Visitor<'_> for __FieldVisitor {
                type Value = __Field;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "field identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => Ok(__Field::__field0),
                        _ => Err(_serde::de::Error::invalid_value(
                            _serde::de::Unexpected::Unsigned(__value),
                            &"field index 0 <= i < 1",
                        )),
                    }
                }
                fn visit_str<__E>(self, __value: &str) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "dependencies" => Ok(__Field::__field0),
                        _ => Ok(__Field::__ignore),
                    }
                }
                fn visit_bytes<__E>(self, __value: &[u8]) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"dependencies" => Ok(__Field::__field0),
                        _ => Ok(__Field::__ignore),
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            struct __Visitor<'de> {
                marker: PhantomData<TomlPlatform>,
                lifetime: PhantomData<&'de ()>,
            }
            impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                type Value = TomlPlatform;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "struct TomlPlatform")
                }
                #[inline]
                fn visit_seq<__A>(self, mut __seq: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::SeqAccess<'de>,
                {
                    let __field0 = match _serde::de::SeqAccess::next_element::<
                        Option<BTreeMap<String, TomlDependency>>,
                    >(&mut __seq)?
                    {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                0usize,
                                &"struct TomlPlatform with 1 element",
                            ));
                        }
                    };
                    Ok(TomlPlatform { dependencies: __field0 })
                }
                #[inline]
                fn visit_map<__A>(self, mut __map: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut __field0: Option<Option<BTreeMap<String, TomlDependency>>> = None;
                    while let Some(__key) = _serde::de::MapAccess::next_key::<__Field>(&mut __map)?
                    {
                        match __key {
                            __Field::__field0 => {
                                if Option::is_some(&__field0) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "dependencies",
                                        ),
                                    );
                                }
                                __field0 = Some(_serde::de::MapAccess::next_value::<
                                    Option<BTreeMap<String, TomlDependency>>,
                                >(&mut __map)?);
                            }
                            _ => {
                                let _ = _serde::de::MapAccess::next_value::<_serde::de::IgnoredAny>(
                                    &mut __map,
                                )?;
                            }
                        }
                    }
                    let __field0 = match __field0 {
                        Some(__field0) => __field0,
                        None => _serde::private::de::missing_field("dependencies")?,
                    };
                    Ok(TomlPlatform { dependencies: __field0 })
                }
            }
            const FIELDS: &[&str] = &["dependencies"];
            _serde::Deserializer::deserialize_struct(
                __deserializer,
                "TomlPlatform",
                FIELDS,
                __Visitor { marker: PhantomData::<TomlPlatform>, lifetime: PhantomData },
            )
        }
    }
    impl _serde::Serialize for TomlPlatform {
        fn serialize<__S>(&self, __serializer: __S) -> Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            let mut __serde_state = _serde::Serializer::serialize_struct(
                __serializer,
                "TomlPlatform",
                false as usize + 1,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "dependencies",
                &self.dependencies,
            )?;
            _serde::ser::SerializeStruct::end(__serde_state)
        }
    }
    impl<'de> _serde::Deserialize<'de> for TomlTarget {
        fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                __field0,
                __field1,
                __field2,
                __field3,
                __field4,
                __field5,
                __field6,
                __field7,
                __field8,
                __field9,
                __field10,
                __field11,
                __ignore,
            }
            struct __FieldVisitor;
            impl _serde::de::Visitor<'_> for __FieldVisitor {
                type Value = __Field;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "field identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => Ok(__Field::__field0),
                        1u64 => Ok(__Field::__field1),
                        2u64 => Ok(__Field::__field2),
                        3u64 => Ok(__Field::__field3),
                        4u64 => Ok(__Field::__field4),
                        5u64 => Ok(__Field::__field5),
                        6u64 => Ok(__Field::__field6),
                        7u64 => Ok(__Field::__field7),
                        8u64 => Ok(__Field::__field8),
                        9u64 => Ok(__Field::__field9),
                        10u64 => Ok(__Field::__field10),
                        11u64 => Ok(__Field::__field11),
                        _ => Err(_serde::de::Error::invalid_value(
                            _serde::de::Unexpected::Unsigned(__value),
                            &"field index 0 <= i < 12",
                        )),
                    }
                }
                fn visit_str<__E>(self, __value: &str) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "name" => Ok(__Field::__field0),
                        "crate-type" => Ok(__Field::__field1),
                        "path" => Ok(__Field::__field2),
                        "test" => Ok(__Field::__field3),
                        "doctest" => Ok(__Field::__field4),
                        "bench" => Ok(__Field::__field5),
                        "doc" => Ok(__Field::__field6),
                        "plugin" => Ok(__Field::__field7),
                        "proc-macro" => Ok(__Field::__field8),
                        "harness" => Ok(__Field::__field9),
                        "required-features" => Ok(__Field::__field10),
                        "edition" => Ok(__Field::__field11),
                        _ => Ok(__Field::__ignore),
                    }
                }
                fn visit_bytes<__E>(self, __value: &[u8]) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"name" => Ok(__Field::__field0),
                        b"crate-type" => Ok(__Field::__field1),
                        b"path" => Ok(__Field::__field2),
                        b"test" => Ok(__Field::__field3),
                        b"doctest" => Ok(__Field::__field4),
                        b"bench" => Ok(__Field::__field5),
                        b"doc" => Ok(__Field::__field6),
                        b"plugin" => Ok(__Field::__field7),
                        b"proc-macro" => Ok(__Field::__field8),
                        b"harness" => Ok(__Field::__field9),
                        b"required-features" => Ok(__Field::__field10),
                        b"edition" => Ok(__Field::__field11),
                        _ => Ok(__Field::__ignore),
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            struct __Visitor<'de> {
                marker: PhantomData<TomlTarget>,
                lifetime: PhantomData<&'de ()>,
            }
            impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                type Value = TomlTarget;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "struct TomlTarget")
                }
                #[inline]
                fn visit_seq<__A>(self, mut __seq: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::SeqAccess<'de>,
                {
                    let __field0 =
                        match _serde::de::SeqAccess::next_element::<Option<String>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    0usize,
                                    &"struct TomlTarget with 12 elements",
                                ));
                            }
                        };
                    let __field1 = match _serde::de::SeqAccess::next_element::<Option<Vec<String>>>(
                        &mut __seq,
                    )? {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                1usize,
                                &"struct TomlTarget with 12 elements",
                            ));
                        }
                    };
                    let __field2 =
                        match _serde::de::SeqAccess::next_element::<Option<PathBuf>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    2usize,
                                    &"struct TomlTarget with 12 elements",
                                ));
                            }
                        };
                    let __field3 =
                        match _serde::de::SeqAccess::next_element::<Option<bool>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    3usize,
                                    &"struct TomlTarget with 12 elements",
                                ));
                            }
                        };
                    let __field4 =
                        match _serde::de::SeqAccess::next_element::<Option<bool>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    4usize,
                                    &"struct TomlTarget with 12 elements",
                                ));
                            }
                        };
                    let __field5 =
                        match _serde::de::SeqAccess::next_element::<Option<bool>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    5usize,
                                    &"struct TomlTarget with 12 elements",
                                ));
                            }
                        };
                    let __field6 =
                        match _serde::de::SeqAccess::next_element::<Option<bool>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    6usize,
                                    &"struct TomlTarget with 12 elements",
                                ));
                            }
                        };
                    let __field7 =
                        match _serde::de::SeqAccess::next_element::<Option<bool>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    7usize,
                                    &"struct TomlTarget with 12 elements",
                                ));
                            }
                        };
                    let __field8 =
                        match _serde::de::SeqAccess::next_element::<Option<bool>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    8usize,
                                    &"struct TomlTarget with 12 elements",
                                ));
                            }
                        };
                    let __field9 =
                        match _serde::de::SeqAccess::next_element::<Option<bool>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    9usize,
                                    &"struct TomlTarget with 12 elements",
                                ));
                            }
                        };
                    let __field10 = match _serde::de::SeqAccess::next_element::<Option<Vec<String>>>(
                        &mut __seq,
                    )? {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                10usize,
                                &"struct TomlTarget with 12 elements",
                            ));
                        }
                    };
                    let __field11 =
                        match _serde::de::SeqAccess::next_element::<Option<String>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    11usize,
                                    &"struct TomlTarget with 12 elements",
                                ));
                            }
                        };
                    Ok(TomlTarget {
                        name: __field0,
                        crate_type: __field1,
                        path: __field2,
                        test: __field3,
                        doctest: __field4,
                        bench: __field5,
                        doc: __field6,
                        plugin: __field7,
                        proc_macro: __field8,
                        harness: __field9,
                        required_features: __field10,
                        edition: __field11,
                    })
                }
                #[inline]
                fn visit_map<__A>(self, mut __map: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut __field0: Option<Option<String>> = None;
                    let mut __field1: Option<Option<Vec<String>>> = None;
                    let mut __field2: Option<Option<PathBuf>> = None;
                    let mut __field3: Option<Option<bool>> = None;
                    let mut __field4: Option<Option<bool>> = None;
                    let mut __field5: Option<Option<bool>> = None;
                    let mut __field6: Option<Option<bool>> = None;
                    let mut __field7: Option<Option<bool>> = None;
                    let mut __field8: Option<Option<bool>> = None;
                    let mut __field9: Option<Option<bool>> = None;
                    let mut __field10: Option<Option<Vec<String>>> = None;
                    let mut __field11: Option<Option<String>> = None;
                    while let Some(__key) = _serde::de::MapAccess::next_key::<__Field>(&mut __map)?
                    {
                        match __key {
                            __Field::__field0 => {
                                if Option::is_some(&__field0) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("name"),
                                    );
                                }
                                __field0 =
                                    Some(_serde::de::MapAccess::next_value::<Option<String>>(
                                        &mut __map,
                                    )?);
                            }
                            __Field::__field1 => {
                                if Option::is_some(&__field1) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "crate-type",
                                        ),
                                    );
                                }
                                __field1 = Some(_serde::de::MapAccess::next_value::<
                                    Option<Vec<String>>,
                                >(&mut __map)?);
                            }
                            __Field::__field2 => {
                                if Option::is_some(&__field2) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("path"),
                                    );
                                }
                                __field2 = Some(_serde::de::MapAccess::next_value::<
                                    Option<PathBuf>,
                                >(&mut __map)?);
                            }
                            __Field::__field3 => {
                                if Option::is_some(&__field3) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("test"),
                                    );
                                }
                                __field3 = Some(_serde::de::MapAccess::next_value::<Option<bool>>(
                                    &mut __map,
                                )?);
                            }
                            __Field::__field4 => {
                                if Option::is_some(&__field4) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "doctest",
                                        ),
                                    );
                                }
                                __field4 = Some(_serde::de::MapAccess::next_value::<Option<bool>>(
                                    &mut __map,
                                )?);
                            }
                            __Field::__field5 => {
                                if Option::is_some(&__field5) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("bench"),
                                    );
                                }
                                __field5 = Some(_serde::de::MapAccess::next_value::<Option<bool>>(
                                    &mut __map,
                                )?);
                            }
                            __Field::__field6 => {
                                if Option::is_some(&__field6) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("doc"),
                                    );
                                }
                                __field6 = Some(_serde::de::MapAccess::next_value::<Option<bool>>(
                                    &mut __map,
                                )?);
                            }
                            __Field::__field7 => {
                                if Option::is_some(&__field7) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "plugin",
                                        ),
                                    );
                                }
                                __field7 = Some(_serde::de::MapAccess::next_value::<Option<bool>>(
                                    &mut __map,
                                )?);
                            }
                            __Field::__field8 => {
                                if Option::is_some(&__field8) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "proc-macro",
                                        ),
                                    );
                                }
                                __field8 = Some(_serde::de::MapAccess::next_value::<Option<bool>>(
                                    &mut __map,
                                )?);
                            }
                            __Field::__field9 => {
                                if Option::is_some(&__field9) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "harness",
                                        ),
                                    );
                                }
                                __field9 = Some(_serde::de::MapAccess::next_value::<Option<bool>>(
                                    &mut __map,
                                )?);
                            }
                            __Field::__field10 => {
                                if Option::is_some(&__field10) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "required-features",
                                        ),
                                    );
                                }
                                __field10 = Some(_serde::de::MapAccess::next_value::<
                                    Option<Vec<String>>,
                                >(&mut __map)?);
                            }
                            __Field::__field11 => {
                                if Option::is_some(&__field11) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "edition",
                                        ),
                                    );
                                }
                                __field11 = Some(_serde::de::MapAccess::next_value::<
                                    Option<String>,
                                >(&mut __map)?);
                            }
                            _ => {
                                let _ = _serde::de::MapAccess::next_value::<_serde::de::IgnoredAny>(
                                    &mut __map,
                                )?;
                            }
                        }
                    }
                    let __field0 = match __field0 {
                        Some(__field0) => __field0,
                        None => _serde::private::de::missing_field("name")?,
                    };
                    let __field1 = match __field1 {
                        Some(__field1) => __field1,
                        None => _serde::private::de::missing_field("crate-type")?,
                    };
                    let __field2 = match __field2 {
                        Some(__field2) => __field2,
                        None => _serde::private::de::missing_field("path")?,
                    };
                    let __field3 = match __field3 {
                        Some(__field3) => __field3,
                        None => _serde::private::de::missing_field("test")?,
                    };
                    let __field4 = match __field4 {
                        Some(__field4) => __field4,
                        None => _serde::private::de::missing_field("doctest")?,
                    };
                    let __field5 = match __field5 {
                        Some(__field5) => __field5,
                        None => _serde::private::de::missing_field("bench")?,
                    };
                    let __field6 = match __field6 {
                        Some(__field6) => __field6,
                        None => _serde::private::de::missing_field("doc")?,
                    };
                    let __field7 = match __field7 {
                        Some(__field7) => __field7,
                        None => _serde::private::de::missing_field("plugin")?,
                    };
                    let __field8 = match __field8 {
                        Some(__field8) => __field8,
                        None => _serde::private::de::missing_field("proc-macro")?,
                    };
                    let __field9 = match __field9 {
                        Some(__field9) => __field9,
                        None => _serde::private::de::missing_field("harness")?,
                    };
                    let __field10 = match __field10 {
                        Some(__field10) => __field10,
                        None => _serde::private::de::missing_field("required-features")?,
                    };
                    let __field11 = match __field11 {
                        Some(__field11) => __field11,
                        None => _serde::private::de::missing_field("edition")?,
                    };
                    Ok(TomlTarget {
                        name: __field0,
                        crate_type: __field1,
                        path: __field2,
                        test: __field3,
                        doctest: __field4,
                        bench: __field5,
                        doc: __field6,
                        plugin: __field7,
                        proc_macro: __field8,
                        harness: __field9,
                        required_features: __field10,
                        edition: __field11,
                    })
                }
            }
            const FIELDS: &[&str] = &[
                "name",
                "crate-type",
                "path",
                "test",
                "doctest",
                "bench",
                "doc",
                "plugin",
                "proc-macro",
                "harness",
                "required-features",
                "edition",
            ];
            _serde::Deserializer::deserialize_struct(
                __deserializer,
                "TomlTarget",
                FIELDS,
                __Visitor { marker: PhantomData::<TomlTarget>, lifetime: PhantomData },
            )
        }
    }
    impl _serde::Serialize for TomlTarget {
        fn serialize<__S>(&self, __serializer: __S) -> Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            let mut __serde_state = _serde::Serializer::serialize_struct(
                __serializer,
                "TomlTarget",
                false as usize + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1,
            )?;
            _serde::ser::SerializeStruct::serialize_field(&mut __serde_state, "name", &self.name)?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "crate-type",
                &self.crate_type,
            )?;
            _serde::ser::SerializeStruct::serialize_field(&mut __serde_state, "path", &self.path)?;
            _serde::ser::SerializeStruct::serialize_field(&mut __serde_state, "test", &self.test)?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "doctest",
                &self.doctest,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "bench",
                &self.bench,
            )?;
            _serde::ser::SerializeStruct::serialize_field(&mut __serde_state, "doc", &self.doc)?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "plugin",
                &self.plugin,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "proc-macro",
                &self.proc_macro,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "harness",
                &self.harness,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "required-features",
                &self.required_features,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "edition",
                &self.edition,
            )?;
            _serde::ser::SerializeStruct::end(__serde_state)
        }
    }
    impl _serde::Serialize for TomlDependency {
        fn serialize<__S>(&self, __serializer: __S) -> Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            match *self {
                TomlDependency::Simple(ref __field0) => {
                    _serde::Serialize::serialize(__field0, __serializer)
                }
                TomlDependency::Detailed(ref __field0) => {
                    _serde::Serialize::serialize(__field0, __serializer)
                }
            }
        }
    }
    impl<'de> _serde::Deserialize<'de> for TomlDependency {
        fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            let __content = <_serde::private::de::Content<'_> as _serde::Deserialize>::deserialize(
                __deserializer,
            )?;
            if let Ok(__ok) = Result::map(
                <String as _serde::Deserialize>::deserialize(
                    _serde::private::de::ContentRefDeserializer::<__D::Error>::new(&__content),
                ),
                TomlDependency::Simple,
            ) {
                return Ok(__ok);
            }
            if let Ok(__ok) = Result::map(
                <DetailedTomlDependency as _serde::Deserialize>::deserialize(
                    _serde::private::de::ContentRefDeserializer::<__D::Error>::new(&__content),
                ),
                TomlDependency::Detailed,
            ) {
                return Ok(__ok);
            }
            Err(_serde::de::Error::custom(
                "data did not match any variant of untagged enum TomlDependency",
            ))
        }
    }
    impl _serde::Serialize for DetailedTomlDependency {
        fn serialize<__S>(&self, __serializer: __S) -> Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            let mut __serde_state = _serde::Serializer::serialize_struct(
                __serializer,
                "DetailedTomlDependency",
                false as usize + 1 + 1 + 1 + 1 + 1,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "version",
                &self.version,
            )?;
            _serde::ser::SerializeStruct::serialize_field(&mut __serde_state, "path", &self.path)?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "features",
                &self.features,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "optional",
                &self.optional,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "default-features",
                &self.default_features,
            )?;
            _serde::ser::SerializeStruct::end(__serde_state)
        }
    }
    impl<'de> _serde::Deserialize<'de> for DetailedTomlDependency {
        fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                __field0,
                __field1,
                __field2,
                __field3,
                __field4,
                __ignore,
            }
            struct __FieldVisitor;
            impl _serde::de::Visitor<'_> for __FieldVisitor {
                type Value = __Field;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "field identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => Ok(__Field::__field0),
                        1u64 => Ok(__Field::__field1),
                        2u64 => Ok(__Field::__field2),
                        3u64 => Ok(__Field::__field3),
                        4u64 => Ok(__Field::__field4),
                        _ => Err(_serde::de::Error::invalid_value(
                            _serde::de::Unexpected::Unsigned(__value),
                            &"field index 0 <= i < 5",
                        )),
                    }
                }
                fn visit_str<__E>(self, __value: &str) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "version" => Ok(__Field::__field0),
                        "path" => Ok(__Field::__field1),
                        "features" => Ok(__Field::__field2),
                        "optional" => Ok(__Field::__field3),
                        "default-features" => Ok(__Field::__field4),
                        _ => Ok(__Field::__ignore),
                    }
                }
                fn visit_bytes<__E>(self, __value: &[u8]) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"version" => Ok(__Field::__field0),
                        b"path" => Ok(__Field::__field1),
                        b"features" => Ok(__Field::__field2),
                        b"optional" => Ok(__Field::__field3),
                        b"default-features" => Ok(__Field::__field4),
                        _ => Ok(__Field::__ignore),
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            struct __Visitor<'de> {
                marker: PhantomData<DetailedTomlDependency>,
                lifetime: PhantomData<&'de ()>,
            }
            impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                type Value = DetailedTomlDependency;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "struct DetailedTomlDependency")
                }
                #[inline]
                fn visit_seq<__A>(self, mut __seq: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::SeqAccess<'de>,
                {
                    let __field0 =
                        match _serde::de::SeqAccess::next_element::<Option<String>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    0usize,
                                    &"struct DetailedTomlDependency with 5 elements",
                                ));
                            }
                        };
                    let __field1 =
                        match _serde::de::SeqAccess::next_element::<Option<String>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    1usize,
                                    &"struct DetailedTomlDependency with 5 elements",
                                ));
                            }
                        };
                    let __field2 = match _serde::de::SeqAccess::next_element::<Option<Vec<String>>>(
                        &mut __seq,
                    )? {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                2usize,
                                &"struct DetailedTomlDependency with 5 elements",
                            ));
                        }
                    };
                    let __field3 =
                        match _serde::de::SeqAccess::next_element::<Option<bool>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    3usize,
                                    &"struct DetailedTomlDependency with 5 elements",
                                ));
                            }
                        };
                    let __field4 =
                        match _serde::de::SeqAccess::next_element::<Option<bool>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    4usize,
                                    &"struct DetailedTomlDependency with 5 elements",
                                ));
                            }
                        };
                    Ok(DetailedTomlDependency {
                        version: __field0,
                        path: __field1,
                        features: __field2,
                        optional: __field3,
                        default_features: __field4,
                    })
                }
                #[inline]
                fn visit_map<__A>(self, mut __map: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut __field0: Option<Option<String>> = None;
                    let mut __field1: Option<Option<String>> = None;
                    let mut __field2: Option<Option<Vec<String>>> = None;
                    let mut __field3: Option<Option<bool>> = None;
                    let mut __field4: Option<Option<bool>> = None;
                    while let Some(__key) = _serde::de::MapAccess::next_key::<__Field>(&mut __map)?
                    {
                        match __key {
                            __Field::__field0 => {
                                if Option::is_some(&__field0) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "version",
                                        ),
                                    );
                                }
                                __field0 =
                                    Some(_serde::de::MapAccess::next_value::<Option<String>>(
                                        &mut __map,
                                    )?);
                            }
                            __Field::__field1 => {
                                if Option::is_some(&__field1) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("path"),
                                    );
                                }
                                __field1 =
                                    Some(_serde::de::MapAccess::next_value::<Option<String>>(
                                        &mut __map,
                                    )?);
                            }
                            __Field::__field2 => {
                                if Option::is_some(&__field2) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "features",
                                        ),
                                    );
                                }
                                __field2 = Some(_serde::de::MapAccess::next_value::<
                                    Option<Vec<String>>,
                                >(&mut __map)?);
                            }
                            __Field::__field3 => {
                                if Option::is_some(&__field3) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "optional",
                                        ),
                                    );
                                }
                                __field3 = Some(_serde::de::MapAccess::next_value::<Option<bool>>(
                                    &mut __map,
                                )?);
                            }
                            __Field::__field4 => {
                                if Option::is_some(&__field4) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "default-features",
                                        ),
                                    );
                                }
                                __field4 = Some(_serde::de::MapAccess::next_value::<Option<bool>>(
                                    &mut __map,
                                )?);
                            }
                            _ => {
                                let _ = _serde::de::MapAccess::next_value::<_serde::de::IgnoredAny>(
                                    &mut __map,
                                )?;
                            }
                        }
                    }
                    let __field0 = match __field0 {
                        Some(__field0) => __field0,
                        None => _serde::private::de::missing_field("version")?,
                    };
                    let __field1 = match __field1 {
                        Some(__field1) => __field1,
                        None => _serde::private::de::missing_field("path")?,
                    };
                    let __field2 = match __field2 {
                        Some(__field2) => __field2,
                        None => _serde::private::de::missing_field("features")?,
                    };
                    let __field3 = match __field3 {
                        Some(__field3) => __field3,
                        None => _serde::private::de::missing_field("optional")?,
                    };
                    let __field4 = match __field4 {
                        Some(__field4) => __field4,
                        None => _serde::private::de::missing_field("default-features")?,
                    };
                    Ok(DetailedTomlDependency {
                        version: __field0,
                        path: __field1,
                        features: __field2,
                        optional: __field3,
                        default_features: __field4,
                    })
                }
            }
            const FIELDS: &[&str] =
                &["version", "path", "features", "optional", "default-features"];
            _serde::Deserializer::deserialize_struct(
                __deserializer,
                "DetailedTomlDependency",
                FIELDS,
                __Visitor { marker: PhantomData::<DetailedTomlDependency>, lifetime: PhantomData },
            )
        }
    }
    impl<'de> _serde::Deserialize<'de> for TomlProject {
        fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            #[allow(non_camel_case_types)]
            enum __Field {
                __field0,
                __field1,
                __field2,
                __field3,
                __ignore,
            }
            struct __FieldVisitor;
            impl _serde::de::Visitor<'_> for __FieldVisitor {
                type Value = __Field;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "field identifier")
                }
                fn visit_u64<__E>(self, __value: u64) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        0u64 => Ok(__Field::__field0),
                        1u64 => Ok(__Field::__field1),
                        2u64 => Ok(__Field::__field2),
                        3u64 => Ok(__Field::__field3),
                        _ => Err(_serde::de::Error::invalid_value(
                            _serde::de::Unexpected::Unsigned(__value),
                            &"field index 0 <= i < 4",
                        )),
                    }
                }
                fn visit_str<__E>(self, __value: &str) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        "edition" => Ok(__Field::__field0),
                        "name" => Ok(__Field::__field1),
                        "version" => Ok(__Field::__field2),
                        "publish" => Ok(__Field::__field3),
                        _ => Ok(__Field::__ignore),
                    }
                }
                fn visit_bytes<__E>(self, __value: &[u8]) -> Result<Self::Value, __E>
                where
                    __E: _serde::de::Error,
                {
                    match __value {
                        b"edition" => Ok(__Field::__field0),
                        b"name" => Ok(__Field::__field1),
                        b"version" => Ok(__Field::__field2),
                        b"publish" => Ok(__Field::__field3),
                        _ => Ok(__Field::__ignore),
                    }
                }
            }
            impl<'de> _serde::Deserialize<'de> for __Field {
                #[inline]
                fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
                where
                    __D: _serde::Deserializer<'de>,
                {
                    _serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                }
            }
            struct __Visitor<'de> {
                marker: PhantomData<TomlProject>,
                lifetime: PhantomData<&'de ()>,
            }
            impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                type Value = TomlProject;
                fn expecting(&self, __formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                    fmt::Formatter::write_str(__formatter, "struct TomlProject")
                }
                #[inline]
                fn visit_seq<__A>(self, mut __seq: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::SeqAccess<'de>,
                {
                    let __field0 =
                        match _serde::de::SeqAccess::next_element::<Option<String>>(&mut __seq)? {
                            Some(__value) => __value,
                            None => {
                                return Err(_serde::de::Error::invalid_length(
                                    0usize,
                                    &"struct TomlProject with 4 elements",
                                ));
                            }
                        };
                    let __field1 = match _serde::de::SeqAccess::next_element::<String>(&mut __seq)?
                    {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                1usize,
                                &"struct TomlProject with 4 elements",
                            ));
                        }
                    };
                    let __field2 = match _serde::de::SeqAccess::next_element::<String>(&mut __seq)?
                    {
                        Some(__value) => __value,
                        None => {
                            return Err(_serde::de::Error::invalid_length(
                                2usize,
                                &"struct TomlProject with 4 elements",
                            ));
                        }
                    };
                    let __field3 = match _serde::de::SeqAccess::next_element::<Publish>(&mut __seq)?
                    {
                        Some(__value) => __value,
                        None => Default::default(),
                    };
                    Ok(TomlProject {
                        edition: __field0,
                        name: __field1,
                        version: __field2,
                        publish: __field3,
                    })
                }
                #[inline]
                fn visit_map<__A>(self, mut __map: __A) -> Result<Self::Value, __A::Error>
                where
                    __A: _serde::de::MapAccess<'de>,
                {
                    let mut __field0: Option<Option<String>> = None;
                    let mut __field1: Option<String> = None;
                    let mut __field2: Option<String> = None;
                    let mut __field3: Option<Publish> = None;
                    while let Some(__key) = _serde::de::MapAccess::next_key::<__Field>(&mut __map)?
                    {
                        match __key {
                            __Field::__field0 => {
                                if Option::is_some(&__field0) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "edition",
                                        ),
                                    );
                                }
                                __field0 =
                                    Some(_serde::de::MapAccess::next_value::<Option<String>>(
                                        &mut __map,
                                    )?);
                            }
                            __Field::__field1 => {
                                if Option::is_some(&__field1) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field("name"),
                                    );
                                }
                                __field1 =
                                    Some(_serde::de::MapAccess::next_value::<String>(&mut __map)?);
                            }
                            __Field::__field2 => {
                                if Option::is_some(&__field2) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "version",
                                        ),
                                    );
                                }
                                __field2 =
                                    Some(_serde::de::MapAccess::next_value::<String>(&mut __map)?);
                            }
                            __Field::__field3 => {
                                if Option::is_some(&__field3) {
                                    return Err(
                                        <__A::Error as _serde::de::Error>::duplicate_field(
                                            "publish",
                                        ),
                                    );
                                }
                                __field3 =
                                    Some(_serde::de::MapAccess::next_value::<Publish>(&mut __map)?);
                            }
                            _ => {
                                let _ = _serde::de::MapAccess::next_value::<_serde::de::IgnoredAny>(
                                    &mut __map,
                                )?;
                            }
                        }
                    }
                    let __field0 = match __field0 {
                        Some(__field0) => __field0,
                        None => _serde::private::de::missing_field("edition")?,
                    };
                    let __field1 = match __field1 {
                        Some(__field1) => __field1,
                        None => _serde::private::de::missing_field("name")?,
                    };
                    let __field2 = match __field2 {
                        Some(__field2) => __field2,
                        None => _serde::private::de::missing_field("version")?,
                    };
                    let __field3 = match __field3 {
                        Some(__field3) => __field3,
                        None => Default::default(),
                    };
                    Ok(TomlProject {
                        edition: __field0,
                        name: __field1,
                        version: __field2,
                        publish: __field3,
                    })
                }
            }
            const FIELDS: &[&str] = &["edition", "name", "version", "publish"];
            _serde::Deserializer::deserialize_struct(
                __deserializer,
                "TomlProject",
                FIELDS,
                __Visitor { marker: PhantomData::<TomlProject>, lifetime: PhantomData },
            )
        }
    }
    impl _serde::Serialize for TomlProject {
        fn serialize<__S>(&self, __serializer: __S) -> Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            let mut __serde_state = _serde::Serializer::serialize_struct(
                __serializer,
                "TomlProject",
                false as usize + 1 + 1 + 1 + 1,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "edition",
                &self.edition,
            )?;
            _serde::ser::SerializeStruct::serialize_field(&mut __serde_state, "name", &self.name)?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "version",
                &self.version,
            )?;
            _serde::ser::SerializeStruct::serialize_field(
                &mut __serde_state,
                "publish",
                &self.publish,
            )?;
            _serde::ser::SerializeStruct::end(__serde_state)
        }
    }
    impl<'de> _serde::Deserialize<'de> for Publish {
        fn deserialize<__D>(__deserializer: __D) -> Result<Self, __D::Error>
        where
            __D: _serde::Deserializer<'de>,
        {
            let __content = <_serde::private::de::Content<'_> as _serde::Deserialize>::deserialize(
                __deserializer,
            )?;
            if let Ok(__ok) = Result::map(
                <bool as _serde::Deserialize>::deserialize(
                    _serde::private::de::ContentRefDeserializer::<__D::Error>::new(&__content),
                ),
                Publish::Flag,
            ) {
                return Ok(__ok);
            }
            if let Ok(__ok) = Result::map(
                <Vec<String> as _serde::Deserialize>::deserialize(
                    _serde::private::de::ContentRefDeserializer::<__D::Error>::new(&__content),
                ),
                Publish::Registry,
            ) {
                return Ok(__ok);
            }
            Err(_serde::de::Error::custom(
                "data did not match any variant of untagged enum Publish",
            ))
        }
    }
    impl _serde::Serialize for Publish {
        fn serialize<__S>(&self, __serializer: __S) -> Result<__S::Ok, __S::Error>
        where
            __S: _serde::Serializer,
        {
            match *self {
                Publish::Flag(ref __field0) => _serde::Serialize::serialize(__field0, __serializer),
                Publish::Registry(ref __field0) => {
                    _serde::Serialize::serialize(__field0, __serializer)
                }
            }
        }
    }
}

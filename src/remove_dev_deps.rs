use std::cmp;

pub(crate) fn remove_dev_deps(bytes: &str) -> String {
    const DEV_DEPS: &str = "dev-dependencies";
    const TARGET: &str = "target.";

    let mut bytes = bytes.to_string();
    let mut prev = 0;
    let mut next = bytes.find('[');

    while let Some(mut pos) = next {
        prev = bytes[prev..pos].rfind('\n').map_or(prev, |n| cmp::min(n + prev + 1, pos));
        // dbg!(&bytes[prev..pos]);

        if bytes[prev..pos].trim().is_empty() {
            let slice = bytes[pos + 1..].trim_start();
            if slice.starts_with(DEV_DEPS) {
                let maybe_close = pos + DEV_DEPS.len();
                if let Some(ahead) = bytes[maybe_close..].find('[') {
                    let back = bytes[maybe_close..maybe_close + ahead]
                        .rfind('\n')
                        .map_or(0, |n| cmp::min(n + 1, ahead));

                    bytes.drain(prev..maybe_close + back);
                    next = Some(prev + ahead - back);
                    continue;
                } else {
                    bytes.drain(prev..);
                    break;
                }
            } else if slice.starts_with(TARGET) {
                let close = bytes[pos + TARGET.len()..].find(']').unwrap() + pos + TARGET.len();
                let mut split = bytes[pos..close].split('.');
                let _ = split.next(); // `target`
                let _ = split.next(); // `'cfg(...)'`
                if let Some(deps) = split.next() {
                    if deps.trim() == DEV_DEPS {
                        if let Some(ahead) = bytes[close..].find('[') {
                            let back = bytes[close..close + ahead]
                                .rfind('\n')
                                .map_or(0, |n| cmp::min(n + 1, ahead));

                            bytes.drain(prev..close + back);
                            next = Some(prev + ahead - back);
                            continue;
                        } else {
                            bytes.drain(prev..);
                            break;
                        }
                    }
                }

                prev = pos;
                next = bytes[close..].find('[');
                continue;
            }
        }

        prev = pos;
        // at least
        // pos + 0 = '['
        // pos + 1 = part of table name (or '[')
        // pos + 2 = ']' (or part of table name)
        // pos + 3 = '\n' or eof (or part of table name or ']')
        // pos + 4 = start of next table or eof (or part of this table)
        pos += 4;
        next = bytes.get(pos..).and_then(|s| s.find('[')).map(|n| pos + n);
        continue;
    }

    bytes
}

#[cfg(test)]
mod tests {
    use toml_e::remove_dev_deps;
    // use super::remove_dev_deps;

    mod toml_e {
        pub(crate) fn remove_dev_deps(raw: &str) -> String {
            let mut doc: toml_edit::Document = raw.parse().unwrap();
            remove_key_and_target_key(doc.as_table_mut(), "dev-dependencies");
            doc.to_string_in_original_order()
        }

        fn remove_key_and_target_key(table: &mut toml_edit::Table, key: &str) {
            table.remove(key);
            if let Some(table) = table.entry("target").as_table_mut() {
                // `toml_edit::Table` does not have `.iter_mut()`, so collect keys.
                for k in table.iter().map(|(key, _)| key.to_string()).collect::<Vec<_>>() {
                    if let Some(table) = table.entry(&k).as_table_mut() {
                        table.remove(key);
                    }
                }
            }
        }
    }

    macro_rules! tes {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let input = remove_dev_deps($input);
                assert_eq!(&$expected[..], input);
                // panic!();
            }
        };
    }

    tes!(
        a,
        "\
[package]
[dependencies]
[[example]]
[dev-dependencies.opencl]
[dev-dependencies]",
        "\
[package]
[dependencies]
[[example]]
"
    );

    tes!(
        b,
        "\
[package]
[dependencies]
[[example]]
[dev-dependencies.opencl]
[dev-dependencies]
",
        "\
[package]
[dependencies]
[[example]]
"
    );

    tes!(
        many_lines,
        "\
[package]\n\n

[dev-dependencies.opencl]


[dev-dependencies]
",
        "\
[package]\n\n

"
    );

    tes!(
        target_deps1,
        "\
[package]

[target.'cfg(unix)'.dev-dependencies]

[dependencies]
",
        "\
[package]

[dependencies]
"
    );

    tes!(
        target_deps2,
        "\
[package]

[target.'cfg(unix)'.dev-dependencies]
foo = \"0.1\"

[target.'cfg(unix)'.dev-dependencies.bar]

[dev-dependencies]
foo = \"0.1\"

[target.'cfg(unix)'.dependencies]
foo = \"0.1\"
",
        "\
[package]

[target.'cfg(unix)'.dependencies]
foo = \"0.1\"
"
    );

    tes!(
        not_table,
        "\
[package]
foo = [dev-dependencies]
# [dev-dependencies]

    [target.'cfg(unix)'.dev-dependencies]
    foo = \"0.1\"

\t[dev-dependencies]
\tfoo = \"0.1\"
",
        "\
[package]
foo = [dev-dependencies]
# [dev-dependencies]

    \t"
    );
}

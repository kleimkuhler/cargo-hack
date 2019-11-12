use std::cmp;

pub(crate) fn remove_dev_deps(bytes: &mut String) {
    const DEV_DEPS: &str = "dev-dependencies";
    const TARGET: &str = "target.";

    let mut prev = 0;
    let mut next = bytes.find('[');

    while let Some(mut pos) = next {
        prev = bytes[prev..pos].rfind('\n').map_or(prev, |n| cmp::min(n + prev + 1, pos));
        dbg!(&bytes[prev..pos]);

        if bytes[prev..pos].trim().is_empty() {
            let slice = bytes[pos + 1..].trim_start();
            if slice.starts_with(DEV_DEPS) {
                let maybe_close = pos + DEV_DEPS.len();
                if let Some(ahead) = bytes[maybe_close..].find('[') {
                    let back = bytes[maybe_close..maybe_close + ahead]
                        .rfind('\n')
                        .map_or(0, |n| cmp::min(n + 1, ahead));

                    bytes.drain(pos..maybe_close + back);
                    next = Some(pos + ahead - back);
                    continue;
                } else {
                    bytes.drain(pos..);
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

                            bytes.drain(pos..close + back);
                            next = Some(pos + ahead - back);
                            continue;
                        } else {
                            bytes.drain(pos..);
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
}

#[cfg(test)]
mod tests {
    use super::remove_dev_deps;

    macro_rules! tes {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let mut buf = $input.to_string();
                remove_dev_deps(&mut buf);
                assert_eq!(&$expected[..], buf);
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

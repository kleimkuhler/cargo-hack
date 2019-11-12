// fn try_main() -> io::Result<()> {
//     let mut buf = Vec::new();
//     args_os().skip(1).try_for_each(|file| {
//         let mut r = BufReader::new(File::open(&file)?);
//         r.read_to_end(&mut buf)?;
//         find(&mut buf);
//         fs::write(file, &buf)?;
//         buf.clear();
//         Ok(())
//     })
// }

// fn find(bytes: &mut Vec<u8>) {
//     const MACRO: &[&[u8]] = &[b"await!(", b"r#await!("];
//     const FEATURE: &[&[u8]] = &[b", await_macro", b"await_macro, ", b"await_macro"];

//     let mut i = 0;
//     while i < bytes.len() {
//         if remove(bytes, i, MACRO) {
//             replace(bytes, i);
//         } else {
//             let _ = remove(bytes, i, FEATURE);
//             i += 1;
//         }
//     }
// }

// fn remove(bytes: &mut Vec<u8>, i: usize, needles: &[&[u8]]) -> bool {
//     for needle in needles {
//         if (&bytes[i..]).starts_with(needle) {
//             bytes.drain(i..i + needle.len());
//             return true;
//         }
//     }
//     false
// }

fn trim_start(bytes: &[u8]) -> &[u8] {
    bytes.iter().position(|&b| !(b as char).is_whitespace()).map_or(&[], |i| &bytes[i..])
}

use memchr::memchr;

// fn memchr(needle: u8, bytes: &[u8]) -> Option<usize> {
//     bytes.iter().position(|&b| b == needle)
// }

pub(crate) fn remove_dev_deps(bytes: &mut Vec<u8>) {
    fn _drain(_bytes: &mut Vec<u8>) -> Option<usize> {
        unimplemented!()
    }

    // const LEN: usize = DEV_DEPS.len();
    const DEV_DEPS: &[u8] = b"dev-dependencies";
    const TARGET: &[u8] = b"target.";

    let mut prev = 0;
    let mut next = memchr(b'[', &bytes);

    while let Some(mut pos) = next {
        let slice = trim_start(&bytes[pos + 1..]);
        if slice.starts_with(DEV_DEPS) {
            if let Some(preview) = memchr(b'[', &bytes[pos + 4..]) {
                bytes.drain(pos..pos + 4 + preview);
                continue;
            } else {
                bytes.drain(pos..);
                break;
            }
        } else if slice.starts_with(TARGET) {
            let close = memchr(b']', &bytes[pos + TARGET.len()..]).unwrap() + pos + TARGET.len();
            let mut split = (&bytes[pos..close]).split(|&b| b == b'.');
            let _ = split.next(); // `target`
            let _ = split.next(); // `'cfg(...)'`
            if let Some(deps) = split.next() {
                if trim_start(deps).starts_with(DEV_DEPS) {
                    if let Some(preview) = memchr(b'[', &bytes[close..]) {
                        bytes.drain(pos..close + preview);
                        continue;
                    } else {
                        bytes.drain(pos..);
                        break;
                    }
                }
            }
            prev = pos;
            next = memchr(b'[', &bytes[close..]);
            continue;
        } else {
            prev = pos;
            // at least
            // pos + 0 = '['
            // pos + 1 = part of table name (or '[')
            // pos + 2 = ']' (or part of table name)
            // pos + 3 = '\n' or eof (or part of table name or ']')
            // pos + 4 = start of next table or eof (or part of this table)
            pos += 4;
            next = bytes.get(pos..).and_then(|b| memchr(b'[', b)).map(|n| pos + n);
            continue;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{remove_dev_deps, trim_start};

    #[test]
    fn test_trim_start() {
        assert_eq!(trim_start(b""), b"");
        assert_eq!(trim_start(b"    "), b"");
        assert_eq!(trim_start(b" \t  \t "), b"");
        assert_eq!(trim_start(b"b"), b"b");
        assert_eq!(trim_start(b"    b"), b"b");
        assert_eq!(trim_start(b" \t  \t b"), b"b");
    }

    #[test]
    fn test_remove_dev_deps() {
        macro_rules! tes {
            ($input:expr, $expected:expr) => {{
                let mut buf = $input.to_vec();
                remove_dev_deps(&mut buf);
                assert_eq!(&$expected[..], std::str::from_utf8(&buf).unwrap());
            }};
        }

        tes!(
            b"\
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
            b"\
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
            b"\
[package]


[dev-dependencies.opencl]


[dev-dependencies]
",
            "\
[package]


"
        );

        tes!(
            b"\
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
            b"\
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
    }
}

/// returns all files in the standard library
pub fn stdlib() -> Vec<(&'static str, &'static str)> {
    vec![("std/ops.pt", include_str!("ops.pt")), ("std/io.pt", include_str!("io.pt"))]
}

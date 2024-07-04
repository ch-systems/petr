/// returns all files in the standard library
pub fn stdlib() -> Vec<(&'static str, &'static str)> {
    vec![
        ("ops.petr", include_str!("ops.petr")),
        ("io.petr", include_str!("io.petr")),
        ("mem.petr", include_str!("mem.petr")),
    ]
}


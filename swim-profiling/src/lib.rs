//! Basic tools and abstractions for profiling performance and timing data and then displaying it.

//! TODO/wishlist:
//! use a .with(fn) pattern to ensure starts and stops always match
//! keep a stack to know which events are sub-events of others, and show that in the
//! table with indentation

use std::{collections::HashMap, time::Duration};

use cli_table::{format::Justify, Cell, Row, RowStruct, Style};

#[derive(Default)]
pub struct Timings {
    entries: HashMap<&'static str, Vec<ProfileEntry>>,
    pending: HashMap<&'static str, Vec<std::time::Instant>>,
}

pub struct ProfileEntry {
    time: Duration,
}

impl Timings {
    pub fn start(
        &mut self,
        key: &'static str,
    ) {
        self.pending.entry(key).or_default().push(std::time::Instant::now());
    }

    pub fn end(
        &mut self,
        key: &'static str,
    ) {
        let Some(entry) = self.pending.get_mut(key) else {
            eprintln!("Profiling error: tried to end event that didn't exist");
            return;
        };
        let Some(start) = entry.pop() else {
            eprintln!("Profiling error: tried to end event that didn't exist");
            return;
        };
        self.entries.entry(key).or_default().push(ProfileEntry { time: start.elapsed() });
    }

    pub fn render(&self) -> String {
        let unended = self.pending.iter().filter(|(k, v)| !v.is_empty()).collect::<Vec<_>>();
        if !unended.is_empty() {
            eprintln!("Profiling error: some events were not ended");
            eprintln!("{:?}", unended);
        }

        // TODO render outliers, median, average, etc etc

        use cli_table::Table;
        use num_format::{Locale, ToFormattedString};

        let mut table = vec![];

        for (key, entries) in &self.entries {
            let total_duration: Duration = entries.iter().map(|e| e.time).sum();
            let duration = match total_duration.as_millis() {
                x if x < 10 => format!("{} ns", total_duration.as_nanos().to_formatted_string(&Locale::en)),
                otherwise => format!("{} ms", otherwise.to_formatted_string(&Locale::en)),
            };
            let duration = duration.cell().justify(Justify::Right);

            let row: RowStruct = vec![key.to_string().cell().bold(true), duration].row();
            table.push(row);
        }

        let table = table
            .table()
            .title(vec!["Event".cell().bold(true), "Total Duration".cell().bold(true)])
            .display()
            .expect("failed to render profiles table");

        format!("{}", table)
    }
}

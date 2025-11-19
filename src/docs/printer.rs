use std::collections::HashSet;

use super::topics::{TOPICS, Topic};

pub fn print_topic_list() {
    println!("Available documentation topics:");
    println!("*Hint - use `prime-lang docs --query fn,if,else,core-syntax`\n");
    for topic in TOPICS {
        println!("- {} [{}]", topic.key, topic.category);
        println!("  {}", topic.title);
        if !topic.aliases.is_empty() {
            println!("  aliases: {}", topic.aliases.join(", "));
        }
        println!();
    }
}

pub fn print_topics(queries: &[String]) -> Result<(), String> {
    let filters = normalize_filters(queries);
    let mut unmatched = filters.clone();
    let mut printed_any = false;
    for topic in TOPICS {
        let should_print = filters.is_empty() || topic.all_keys().any(|key| filters.contains(key));
        if should_print {
            printed_any = true;
            print_topic(topic);
            if !filters.is_empty() {
                for key in topic.all_keys() {
                    unmatched.remove(key);
                }
            }
        }
    }
    if !filters.is_empty() && !unmatched.is_empty() {
        let mut missing: Vec<_> = unmatched.into_iter().collect();
        missing.sort();
        return Err(format!(
            "unknown documentation topic(s): {}",
            missing.join(", ")
        ));
    }
    if !printed_any {
        println!("No documentation topics matched the request.");
    }
    Ok(())
}

fn print_topic(topic: &Topic) {
    println!(
        "== {} ({} · {}) ==\n{}\n",
        topic.title, topic.key, topic.category, topic.summary
    );
    for section in topic.sections {
        println!("-- {} --", section.title);
        println!("```prime\n{}\n```", section.snippet.trim_end());
        println!("{}\n", section.explanation);
    }
}

fn normalize_filters(raw: &[String]) -> HashSet<String> {
    let mut filters = HashSet::new();
    for value in raw {
        for part in value.split(',') {
            let token = part.trim().to_lowercase();
            if !token.is_empty() {
                filters.insert(token);
            }
        }
    }
    filters
}

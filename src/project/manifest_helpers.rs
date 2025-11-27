use toml::Value;

#[allow(dead_code)]
pub fn get_items_array(value: Option<&Value>) -> Option<Value> {
    match value {
        Some(Value::Table(table)) => table.get("items").cloned(),
        other => other.cloned(),
    }
}

pub fn entries_from_value(value: Option<&Value>) -> Vec<Value> {
    match value {
        Some(Value::Array(entries)) => entries.clone(),
        Some(Value::Table(table)) => {
            let mut out = Vec::new();
            if let Some(items) = table.get("items").and_then(|v| v.as_array()) {
                out.extend(items.clone());
            }
            for (key, v) in table {
                if key == "items" {
                    continue;
                }
                if let Some(tbl) = v.as_table() {
                    let mut m = tbl.clone();
                    if !m.contains_key("name") {
                        m.insert("name".into(), Value::String(key.to_string()));
                    }
                    out.push(Value::Table(m));
                    continue;
                }
                if let Some(path) = v.as_str() {
                    let mut m = toml::map::Map::new();
                    m.insert("name".into(), Value::String(key.to_string()));
                    m.insert("path".into(), Value::String(path.to_string()));
                    out.push(Value::Table(m));
                    continue;
                }
            }
            out
        }
        _ => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_items_prefers_table_items() {
        let mut table = toml::map::Map::new();
        table.insert(
            "items".into(),
            Value::Array(vec![Value::Table({
                let mut t = toml::map::Map::new();
                t.insert("name".into(), Value::String("demo".into()));
                t
            })]),
        );
        let result =
            get_items_array(Some(&Value::Table(table))).and_then(|v| v.as_array().cloned());
        assert!(result.is_some());
    }

    #[test]
    fn entries_from_table_infers_name_for_strings() {
        let mut table = toml::map::Map::new();
        table.insert("demo".into(), Value::String("demo.prime".into()));
        let entries = entries_from_value(Some(&Value::Table(table)));
        assert_eq!(entries.len(), 1);
        let first = entries[0].as_table().expect("table entry");
        assert_eq!(first.get("name").and_then(|v| v.as_str()), Some("demo"));
        assert_eq!(
            first.get("path").and_then(|v| v.as_str()),
            Some("demo.prime")
        );
    }
}

use crate::{
    language::{
        span::Span,
        token::{Token, TokenKind},
    },
    project::{find_manifest, manifest::PackageManifest},
};
use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};
use tower_lsp_server::ls_types::{Position, Range, Uri};

pub fn token_at(tokens: &[Token], offset: usize) -> Option<&Token> {
    tokens
        .iter()
        .find(|token| offset >= token.span.start && offset < token.span.end)
}

pub fn identifier_at(tokens: &[Token], offset: usize) -> Option<(String, Span)> {
    token_at(tokens, offset).and_then(|token| match &token.kind {
        TokenKind::Identifier(name) => Some((name.clone(), token.span)),
        _ => None,
    })
}

fn collect_identifier_spans_filtered<F>(tokens: &[Token], target: &str, mut filter: F) -> Vec<Span>
where
    F: FnMut(Span) -> bool,
{
    let mut spans: Vec<Span> = tokens
        .iter()
        .filter_map(|token| match &token.kind {
            TokenKind::Identifier(name) if name == target && filter(token.span) => Some(token.span),
            _ => None,
        })
        .collect();
    spans.sort_by(|a, b| a.start.cmp(&b.start).then_with(|| a.end.cmp(&b.end)));
    spans.dedup();
    spans
}

pub fn collect_identifier_spans(tokens: &[Token], target: &str) -> Vec<Span> {
    collect_identifier_spans_filtered(tokens, target, |_| true)
}

pub fn collect_identifier_spans_in_scope(tokens: &[Token], target: &str, scope: Span) -> Vec<Span> {
    collect_identifier_spans_filtered(tokens, target, |span| {
        span.start >= scope.start && span.start < scope.end
    })
}

pub fn is_valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => (),
        _ => return false,
    }
    chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
}

pub fn span_to_range(text: &str, span: Span) -> Range {
    let len = text.len();
    let start_offset = span.start.min(len);
    let end_offset = span.end.min(len);
    if start_offset < end_offset {
        Range {
            start: offset_to_position(text, start_offset),
            end: offset_to_position(text, end_offset),
        }
    } else {
        let reference = adjust_zero_length_offset(text, start_offset);
        let (line_start, line_end) = line_bounds_at(text, reference);
        Range {
            start: offset_to_position(text, line_start),
            end: offset_to_position(text, line_end),
        }
    }
}

pub fn full_range(text: &str) -> Range {
    Range {
        start: Position::new(0, 0),
        end: offset_to_position(text, text.len()),
    }
}

// LSP lines exclude their terminators. CRLF is one line ending.
fn document_lines(text: &str) -> impl Iterator<Item = (usize, &str)> {
    let mut next = Some(0);
    std::iter::from_fn(move || {
        let start = next?;
        let tail = &text[start..];
        let length = tail.find(['\r', '\n']).unwrap_or(tail.len());
        let ending = &tail[length..];
        next = if ending.is_empty() {
            None
        } else {
            Some(start + length + if ending.starts_with("\r\n") { 2 } else { 1 })
        };
        Some((start, &tail[..length]))
    })
}

pub fn offset_to_position(text: &str, offset: usize) -> Position {
    let offset = text.floor_char_boundary(offset.min(text.len()));
    let (number, (start, line)) = document_lines(text)
        .enumerate()
        .take_while(|(_, (start, _))| *start <= offset)
        .last()
        .expect("every document has a first line");
    let end = (offset - start).min(line.len());
    Position::new(number as u32, line[..end].encode_utf16().count() as u32)
}

pub fn position_to_offset(text: &str, position: Position) -> usize {
    let Some((start, line)) = document_lines(text).nth(position.line as usize) else {
        return text.len();
    };
    let mut remaining = position.character as usize;
    for (byte, ch) in line.char_indices() {
        if remaining < ch.len_utf16() {
            return start + byte;
        }
        remaining -= ch.len_utf16();
    }
    start + line.len()
}

pub fn range_to_offsets(text: &str, range: &Range) -> Option<(usize, usize)> {
    let start = position_to_offset(text, range.start);
    let end = position_to_offset(text, range.end);
    if start <= end && end <= text.len() {
        Some((start, end))
    } else {
        None
    }
}

pub fn prefix_identifier(text: &str, range: &Range) -> String {
    if let Some((start, end)) = range_to_offsets(text, range) {
        let mut new_text = text[start..end].to_string();
        if !new_text.starts_with('_') {
            new_text.insert(0, '_');
        }
        new_text
    } else {
        String::new()
    }
}

pub fn identifier_prefix_slice(text: &str, offset: usize) -> Option<&str> {
    if text.is_empty() {
        return None;
    }
    let len = text.len();
    let mut end = offset.min(len);
    while end > 0 && !text.is_char_boundary(end) {
        end -= 1;
    }
    let bytes = text.as_bytes();
    let mut start = end;
    while start > 0 {
        let ch = bytes[start - 1];
        if is_ident_char(ch) {
            start -= 1;
        } else {
            break;
        }
    }
    if start == end {
        None
    } else {
        text.get(start..end)
    }
}

pub fn prefix_matches(name: &str, prefix: Option<&str>) -> bool {
    match prefix {
        Some(prefix) => name.starts_with(prefix),
        None => true,
    }
}

pub fn is_ident_string(value: &str) -> bool {
    !value.is_empty() && value.bytes().all(is_ident_char)
}

pub fn extract_text(text: &str, start: usize, end: usize) -> String {
    let len = text.len();
    if start >= len || start >= end {
        return String::new();
    }
    let end = end.min(len);
    text[start..end].trim().to_string()
}

pub fn first_non_whitespace_span(text: &str) -> Span {
    for (idx, ch) in text.char_indices() {
        if !ch.is_whitespace() {
            let end = idx + ch.len_utf8();
            return Span::new(idx, end);
        }
    }
    let end = text.chars().next().map(|ch| ch.len_utf8()).unwrap_or(0);
    Span::new(0, end)
}

pub fn url_to_path(url: &Uri) -> Option<PathBuf> {
    url.to_file_path().map(|cow: Cow<'_, Path>| match cow {
        Cow::Owned(path) => path,
        Cow::Borrowed(path) => path.to_path_buf(),
    })
}

pub fn manifest_context_for_uri(uri: &Uri) -> Option<(PackageManifest, PathBuf)> {
    let path = url_to_path(uri)?;
    let manifest_path = find_manifest(&path)?;
    let manifest = PackageManifest::load(&manifest_path).ok()?;
    Some((manifest, path))
}

pub fn adjust_zero_length_offset(text: &str, offset: usize) -> usize {
    if text.is_empty() {
        return 0;
    }
    let len = text.len();
    let mut idx = offset.min(len);
    if idx > 0 {
        idx = prev_char_boundary(text, idx);
    }
    let prefix = &text[..idx];
    for (byte_idx, ch) in prefix.char_indices().rev() {
        if matches!(ch, '\r' | '\n') {
            return byte_idx.saturating_sub(1);
        }
        if !ch.is_whitespace() {
            return byte_idx;
        }
    }
    0
}

fn prev_char_boundary(text: &str, mut idx: usize) -> usize {
    let len = text.len();
    if idx > len {
        idx = len;
    }
    while idx > 0 && !text.is_char_boundary(idx) {
        idx -= 1;
    }
    idx
}

fn line_bounds_at(text: &str, offset: usize) -> (usize, usize) {
    let (start, line) = document_lines(text)
        .take_while(|(start, _)| *start <= offset)
        .last()
        .expect("every document has a first line");
    (start, start + line.len())
}

pub fn error_with_context(base: &str, help: Option<&str>, _text: &str, _span: Span) -> String {
    match help {
        Some(help) if !help.trim().is_empty() => {
            let mut msg = base.to_string();
            msg.push('\n');
            msg.push_str(help);
            msg
        }
        _ => base.to_string(),
    }
}

pub fn prettify_error_message(message: &str) -> String {
    match message {
        "Expected Semi" => "Expected Semicolon ';'".to_string(),
        other => other.to_string(),
    }
}

pub fn is_ident_char(ch: u8) -> bool {
    ch.is_ascii_alphanumeric() || ch == b'_'
}

#[cfg(test)]
mod position_tests {
    use super::*;

    #[test]
    fn utf16_columns_and_byte_spans_round_trip_across_line_endings() {
        let text = "ž😀a\r\n中\rx\n";
        for (byte, line, column) in [
            (0, 0, 0),
            (2, 0, 1),
            (6, 0, 3),
            (7, 0, 4),
            (9, 1, 0),
            (12, 1, 1),
            (13, 2, 0),
            (15, 3, 0),
        ] {
            let position = Position::new(line, column);
            assert_eq!(position_to_offset(text, position), byte);
            assert_eq!(offset_to_position(text, byte), position);
        }
        assert_eq!(offset_to_position(text, 8), Position::new(0, 4));
        assert_eq!(position_to_offset(text, Position::new(0, 99)), 7);
        assert_eq!(position_to_offset(text, Position::new(0, 2)), 2);
        assert_eq!(position_to_offset(text, Position::new(9, 0)), text.len());
    }
}

use super::*;

#[derive(Debug)]
pub(super) struct CallContext {
    pub(super) name: String,
    pub(super) arg_index: usize,
}

pub(super) fn call_context(text: &str, offset: usize) -> Option<CallContext> {
    if text.is_empty() {
        return None;
    }
    let mut idx = offset.min(text.len());
    let bytes = text.as_bytes();
    let mut depth = 0i32;
    while idx > 0 {
        idx -= 1;
        match bytes[idx] {
            b')' | b']' | b'}' => depth += 1,
            b'(' => {
                if depth == 0 {
                    let mut end = idx;
                    while end > 0 && bytes[end - 1].is_ascii_whitespace() {
                        end -= 1;
                    }
                    let mut start = end;
                    while start > 0
                        && (bytes[start - 1].is_ascii_alphanumeric() || bytes[start - 1] == b'_')
                    {
                        start -= 1;
                    }
                    if start == end {
                        return None;
                    }
                    let name = text[start..end].to_string();
                    let args_slice = &text[idx + 1..offset];
                    let arg_index = argument_index(args_slice);
                    return Some(CallContext { name, arg_index });
                } else {
                    depth -= 1;
                }
            }
            _ => {}
        }
    }
    None
}

fn argument_index(segment: &str) -> usize {
    let mut depth = 0i32;
    let mut count = 0usize;
    for ch in segment.chars() {
        match ch {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => {
                if depth > 0 {
                    depth -= 1;
                }
            }
            ',' if depth == 0 => count += 1,
            _ => {}
        }
    }
    count
}

pub(super) fn signature_help_from_modules(
    modules: &[Module],
    ctx: &CallContext,
) -> Option<SignatureHelp> {
    let mut signatures = Vec::new();
    for module in modules {
        for item in &module.items {
            if let Item::Function(func) = item {
                if func.name == ctx.name {
                    let params_info = func
                        .params
                        .iter()
                        .map(|param| ParameterInformation {
                            label: ParameterLabel::Simple(format_function_param(param)),
                            documentation: None,
                        })
                        .collect();
                    signatures.push(SignatureInformation {
                        label: format_function_signature(func),
                        documentation: None,
                        parameters: Some(params_info),
                        active_parameter: None,
                    });
                }
            }
        }
    }
    if signatures.is_empty() {
        return None;
    }
    let active_param = signatures[0]
        .parameters
        .as_ref()
        .map(|params| ctx.arg_index.min(params.len().saturating_sub(1)));
    Some(SignatureHelp {
        signatures,
        active_signature: Some(0),
        active_parameter: active_param.map(|idx| idx as u32),
    })
}

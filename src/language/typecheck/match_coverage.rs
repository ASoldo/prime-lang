//! Exhaustiveness of enum payloads. Each row is one unguarded arm. Keeping
//! fields in the same row preserves constraints such as (true, false).
use super::*;

type Cell<'a> = Vec<&'a Pattern>;
type Row<'a> = Vec<Cell<'a>>;
const ANY: &Pattern = &Pattern::Wildcard;

fn unrestricted(p: &Pattern) -> bool {
    match p {
        Pattern::Wildcard | Pattern::Identifier(..) => true,
        Pattern::Map(entries, _) => entries.is_empty(),
        _ => false,
    }
}

pub(super) fn missing_variants<'d>(
    checker: &Checker,
    env: &FnEnv,
    def: &'d EnumDef,
    args: &[TypeExpr],
    patterns: &[&Pattern],
) -> Vec<&'d str> {
    let coverage = Coverage { checker, env };
    let rows: Vec<_> = patterns.iter().map(|p| vec![vec![*p]]).collect();
    def.variants
        .iter()
        .filter(|v| !coverage.variant(def, args, v, &[], &rows))
        .map(|v| v.name.as_str())
        .collect()
}

struct Coverage<'a> {
    checker: &'a Checker,
    env: &'a FnEnv,
}

impl Coverage<'_> {
    fn variant(
        &self,
        def: &EnumDef,
        args: &[TypeExpr],
        variant: &EnumVariant,
        tail: &[TypeExpr],
        rows: &[Row<'_>],
    ) -> bool {
        let fields: Vec<_> = variant_fields(def, args, variant)
            .into_iter()
            .map(|f| f.ty)
            .collect();
        self.case(&fields, tail, rows, |p| match p {
            Pattern::EnumVariant {
                variant: name,
                bindings,
                ..
            } if name == &variant.name && bindings.len() == fields.len() => {
                Some(bindings.iter().enumerate().collect())
            }
            _ => None,
        })
    }

    // Fix one constructor, then check its fields together with all remaining columns.
    fn case<'p>(
        &self,
        fields: &[TypeExpr],
        tail: &[TypeExpr],
        rows: &[Row<'p>],
        arguments: impl Fn(&'p Pattern) -> Option<Vec<(usize, &'p Pattern)>>,
    ) -> bool {
        let specialized: Vec<_> = rows
            .iter()
            .filter_map(|row| {
                let mut args = vec![Vec::new(); fields.len()];
                for pattern in row[0].iter().filter(|p| !unrestricted(p)) {
                    for (index, constraint) in arguments(pattern)? {
                        args[index].push(constraint);
                    }
                }
                args.extend_from_slice(&row[1..]);
                Some(args)
            })
            .collect();
        self.covers(&[fields, tail].concat(), &specialized)
    }

    fn covers(&self, types: &[TypeExpr], rows: &[Row<'_>]) -> bool {
        if rows.is_empty() {
            return false;
        }
        if rows
            .iter()
            .any(|row| row.iter().flatten().all(|p| unrestricted(p)))
        {
            return true;
        }
        let (ty, tail) = types.split_first().unwrap();
        let defaults: Vec<_> = rows
            .iter()
            .filter(|r| r[0].iter().all(|p| unrestricted(p)))
            .map(|r| r[1..].to_vec())
            .collect();
        if self.covers(tail, &defaults) {
            return true;
        }
        if defaults.len() == rows.len() {
            return false;
        }
        // The interpreter also matches enum variants through one reference.
        let ty = match ty {
            TypeExpr::Reference { ty, .. } => ty.as_ref(),
            other => other,
        };
        match ty {
            TypeExpr::Named(name, _) if name == "bool" => [true, false].into_iter().all(|value| {
                self.case(&[], tail, rows, |p| match p {
                    Pattern::Literal(Literal::Bool(v, _)) if *v == value => Some(vec![]),
                    _ => None,
                })
            }),
            TypeExpr::Named(name, args) => {
                if let Some(info) = self
                    .checker
                    .lookup_enum(self.env, name, &mut false)
                    .or_else(|| self.checker.registry.enums.get(name).cloned())
                {
                    return info
                        .def
                        .variants
                        .iter()
                        .all(|v| self.variant(&info.def, args, v, tail, rows));
                }
                if let Some(info) = self.checker.registry.structs.get(name) {
                    return self.struct_fields(&info.def, args, tail, rows);
                }
                // Enum construction in the interpreter keeps the full integer
                // value even for a narrow annotation such as uint8.
                false
            }
            TypeExpr::Tuple(fields) => self.case(fields, tail, rows, |p| match p {
                Pattern::Tuple(elements, _) if elements.len() == fields.len() => {
                    Some(elements.iter().enumerate().collect())
                }
                _ => None,
            }),
            TypeExpr::Slice(element) => self.slice_lengths(element, tail, rows),
            _ => false,
        }
    }

    fn struct_fields(
        &self,
        def: &StructDef,
        args: &[TypeExpr],
        tail: &[TypeExpr],
        rows: &[Row<'_>],
    ) -> bool {
        // Multiple constraints on a named field must apply to the same value.
        let names: std::collections::BTreeSet<_> = rows
            .iter()
            .flat_map(|row| &row[0])
            .filter_map(|p| {
                if let Pattern::Struct { fields, .. } = p {
                    Some(fields)
                } else {
                    None
                }
            })
            .flatten()
            .map(|f| f.name.as_str())
            .collect();
        let names: Vec<_> = names.into_iter().collect();
        let types: Option<Vec<_>> = names
            .iter()
            .map(|name| struct_pattern_field(&self.checker.registry, def, args, name))
            .collect();
        let Some(types) = types else { return false };
        self.case(&types, tail, rows, |p| {
            let Pattern::Struct { fields, .. } = p else {
                return None;
            };
            Some(
                fields
                    .iter()
                    .map(|f| (names.binary_search(&f.name.as_str()).unwrap(), &f.pattern))
                    .collect(),
            )
        })
    }

    fn slice_lengths(&self, element: &TypeExpr, tail: &[TypeExpr], rows: &[Row<'_>]) -> bool {
        let shapes: Vec<_> = rows
            .iter()
            .flat_map(|r| &r[0])
            .filter_map(|p| slice_parts(p))
            .collect();
        let prefix = shapes.iter().map(|(p, _, _)| p.len()).max().unwrap_or(0);
        let suffix = shapes.iter().map(|(_, _, s)| s.len()).max().unwrap_or(0);
        // Beyond this length, every prefix and suffix is disjoint and all
        // additional middle elements are unconstrained by every matching row.
        (0..=prefix + suffix + 1).all(|len| {
            self.case(&vec![element.clone(); len], tail, rows, |p| {
                let (mut left, variadic, right) = slice_parts(p)?;
                let fixed = left.len() + right.len();
                if fixed > len || (!variadic && fixed != len) {
                    return None;
                }
                left.resize(len - right.len(), ANY);
                left.extend(right);
                Some(left.into_iter().enumerate().collect())
            })
        })
    }
}

// Flatten a nested rest pattern such as [first, ..[second, ..tail], last].
fn slice_parts(mut p: &Pattern) -> Option<(Vec<&Pattern>, bool, Vec<&Pattern>)> {
    let mut left = Vec::new();
    let mut right = Vec::new();
    let variadic = loop {
        match p {
            Pattern::Slice {
                prefix,
                rest,
                suffix,
                ..
            } => {
                left.extend(prefix);
                right.extend(suffix.iter().rev());
                match rest.as_deref() {
                    Some(next) => p = next,
                    None => break false,
                }
            }
            Pattern::Wildcard | Pattern::Identifier(..) => break true,
            _ => return None,
        }
    };
    right.reverse();
    Some((left, variadic, right))
}

//! Semantic facts for the expanded nodes that produced them. A checked program
//! borrows the AST so code generation cannot accidentally pair two revisions.
use super::*;

pub struct CheckedProgram<'a> {
    pub(crate) program: &'a Program,
    pub(crate) target: BuildTarget,
    pub(crate) facts: CheckedFacts,
}

impl CheckedProgram<'_> {
    pub fn program(&self) -> &Program {
        self.program
    }

    pub fn facts(&self) -> &CheckedFacts {
        &self.facts
    }
}

#[derive(Default)]
pub struct CheckedFacts {
    pub(crate) integers: crate::language::integer::CheckedIntegers,
    pub(crate) matches: HashMap<ExprId, CheckedMatch>,
}

impl CheckedFacts {
    pub fn integers(&self) -> &crate::language::integer::CheckedIntegers {
        &self.integers
    }

    pub fn match_expr(&self, id: ExprId) -> Option<&CheckedMatch> {
        self.matches.get(&id)
    }
}

#[derive(Clone, Debug)]
pub struct CheckedMatch {
    pub is_enum: bool,
    pub path: PathBuf,
    pub span: Span,
    pub scrutinee: TypeAnnotation,
    pub arms: Vec<CheckedMatchArm>,
}

#[derive(Clone, Debug)]
pub struct CheckedMatchArm {
    pub variant: Option<CheckedVariant>,
    pub guard: Option<TypeAnnotation>,
    pub value: Option<TypeAnnotation>,
}

#[derive(Clone, Debug)]
pub struct CheckedVariant {
    pub enum_name: String,
    pub name: String,
    pub tag: u32,
    pub fields: Vec<TypeAnnotation>,
}

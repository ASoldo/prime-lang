use super::*;
use crate::language::span::{Span, Spanned};
use crate::language::{ast::Program, parser::parse_module};
use crate::project::load_package;
use std::{
    collections::HashMap,
    env,
    path::{Path, PathBuf},
    ptr,
    sync::{Arc, Mutex, OnceLock},
    thread,
};

fn compile_source(source: &str) -> Result<(), String> {
    let module =
        parse_module("tests::build", PathBuf::from("test.prime"), source).expect("parse");
    let program = Program {
        modules: vec![module],
    };
    let mut compiler = Compiler::new();
    compiler.compile_program(&program)
}

fn with_rt_handles<T>(f: impl FnOnce() -> T) -> T {
    static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    let guard = ENV_LOCK.get_or_init(|| Mutex::new(())).lock().unwrap();
    let prior = env::var("PRIME_ENABLE_RT_HANDLES").ok();
    unsafe {
        env::set_var("PRIME_ENABLE_RT_HANDLES", "1");
    }
    let result = f();
    if let Some(val) = prior {
        unsafe {
            env::set_var("PRIME_ENABLE_RT_HANDLES", val);
        }
    } else {
        unsafe {
            env::remove_var("PRIME_ENABLE_RT_HANDLES");
        }
    }
    drop(guard);
    result
}

fn with_build_parallel<T>(f: impl FnOnce() -> T) -> T {
    static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    let guard = ENV_LOCK.get_or_init(|| Mutex::new(())).lock().unwrap();
    let prior = env::var("PRIME_BUILD_PARALLEL").ok();
    unsafe {
        env::set_var("PRIME_BUILD_PARALLEL", "1");
    }
    let result = f();
    if let Some(val) = prior {
        unsafe {
            env::set_var("PRIME_BUILD_PARALLEL", val);
        }
    } else {
        unsafe {
            env::remove_var("PRIME_BUILD_PARALLEL");
        }
    }
    drop(guard);
    result
}

#[test]
fn build_snapshot_captures_basic_constants() {
    let mut compiler = Compiler::new();
    compiler.push_scope();
    let int_binding = Binding {
        cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Int(
            IntValue::new(ptr::null_mut(), Some(7)),
        )))),
        mutable: false,
        slot: None,
    };
    let tuple_binding = Binding {
        cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Tuple(vec![
            Value::Bool(compiler.const_bool_value(true)),
            Value::Int(IntValue::new(ptr::null_mut(), Some(2))),
        ])))),
        mutable: true,
        slot: None,
    };
    let string_binding = Binding {
        cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Str(
            StringValue::new(ptr::null_mut(), Arc::new("hello".to_string())),
        )))),
        mutable: false,
        slot: None,
    };
    if let Some(scope) = compiler.scopes.last_mut() {
        scope.insert("seven".into(), int_binding);
        scope.insert("pair".into(), tuple_binding);
        scope.insert("greeting".into(), string_binding);
    }

    let snapshot = compiler
        .snapshot_build_state()
        .expect("snapshot should capture constants");
    assert_eq!(snapshot.scopes.len(), 1);
    let bindings = &snapshot.scopes[0].bindings;
    match bindings
        .get("seven")
        .and_then(|b| b.cell.lock().ok())
        .map(|v| v.clone())
    {
        Some(BuildValue::Int(value)) => assert_eq!(value, 7),
        other => panic!("unexpected binding for seven: {:?}", other),
    }
    match bindings
        .get("pair")
        .and_then(|b| b.cell.lock().ok())
        .map(|v| v.clone())
    {
        Some(BuildValue::Tuple(items)) => {
            assert_eq!(items.len(), 2);
            assert!(matches!(items[0], BuildValue::Bool(true)));
            assert!(matches!(items[1], BuildValue::Int(2)));
        }
        other => panic!("unexpected binding for pair: {:?}", other),
    }
    match bindings
        .get("greeting")
        .and_then(|b| b.cell.lock().ok())
        .map(|v| v.clone())
    {
        Some(BuildValue::String(text)) => assert_eq!(text, "hello"),
        other => panic!("unexpected binding for greeting: {:?}", other),
    }
    assert!(bindings.get("pair").map(|b| b.mutable).unwrap_or_default());
}

#[test]
fn build_snapshot_rejects_non_constant_values() {
    let mut compiler = Compiler::new();
    compiler.push_scope();
    let non_const_int = Binding {
        cell: Rc::new(Mutex::new(EvaluatedValue::from_value(Value::Int(
            IntValue::new(ptr::null_mut(), None),
        )))),
        mutable: false,
        slot: None,
    };
    if let Some(scope) = compiler.scopes.last_mut() {
        scope.insert("ephemeral".into(), non_const_int);
    }

    let snapshot_err = compiler.snapshot_build_state().unwrap_err();
    assert!(
        snapshot_err.contains("Non-constant integer"),
        "unexpected error: {snapshot_err}"
    );
}

#[test]
fn for_range_accepts_dynamic_bounds() {
    let source = r#"
module tests::dynamic_range;

fn loop_from(n: int32) {
  for i in n..(n + 3) {
out(i);
  }
}

fn main() { loop_from(2); }
"#;
    let result = compile_source(source);
    assert!(result.is_ok(), "{}", result.unwrap_err());
}

#[test]
fn compiler_releases_borrows_across_control_flow() {
    let source = r#"
module tests::build;

fn release_after_if() {
  let mut int32 value = 0;
  if true {
let &mut int32 alias = &mut value;
*alias = 1;
  } else {
let &mut int32 alias = &mut value;
*alias = 2;
  }
  let &mut int32 after_if = &mut value;
  *after_if = 3;
}

fn release_after_match() {
  let mut int32 value = 0;
  match true {
true => {
  let &mut int32 alias = &mut value;
  *alias = 4;
},
false => {
  let &mut int32 alias = &mut value;
  *alias = 5;
},
  }
  let &mut int32 after_match = &mut value;
  *after_match = 6;
}

fn release_after_while() {
  let mut int32 value = 0;
  let mut int32 idx = 0;
  while idx < 1 {
let &mut int32 alias = &mut value;
*alias = idx;
idx = idx + 1;
  }
  let &mut int32 after = &mut value;
  *after = 7;
}

fn release_after_while_let() {
  let mut int32 value = 0;
  let mut int32 idx = 0;
  while let true = idx == 0 {
let &mut int32 alias = &mut value;
*alias = idx;
idx = idx + 1;
  }
  let &mut int32 after = &mut value;
  *after = 8;
}

fn release_after_for_range() {
  let mut int32 value = 0;
  for count in 0..1 {
let &mut int32 alias = &mut value;
*alias = count;
  }
  let &mut int32 after = &mut value;
  *after = 9;
}

fn release_after_for_collection() {
  let []int32 items = [1, 2];
  let mut int32 value = 0;
  for entry in items {
let &mut int32 alias = &mut value;
*alias = entry;
  }
  let &mut int32 after = &mut value;
  *after = 10;
}

fn release_in_nested_block() {
  let mut int32 value = 0;
  {
let &mut int32 alias = &mut value;
*alias = 11;
  }
  let &mut int32 after = &mut value;
  *after = 12;
}

fn release_after_early_return(flag: bool) -> int32 {
  let mut int32 value = 0;
  if flag {
let &mut int32 alias = &mut value;
*alias = 13;
return value;
  }
  let &mut int32 final_ref = &mut value;
  *final_ref = 14;
  value
}

fn release_after_nested_match() {
  let mut int32 value = 0;
  match true {
true => {
  match false {
    true => {
      let &mut int32 alias = &mut value;
      *alias = 15;
    },
    false => {
      let &mut int32 alias = &mut value;
      *alias = 16;
    },
  }
},
false => {},
  }
  let &mut int32 after = &mut value;
  *after = 17;
}

fn release_after_defer() {
  let mut int32 value = 0;
  {
defer {
  let &mut int32 alias = &mut value;
  *alias = 18;
};
  }
  let &mut int32 after = &mut value;
  *after = 19;
}

fn main() {
  release_after_if();
  release_after_match();
  release_after_while();
  release_after_while_let();
  release_after_for_range();
  release_after_for_collection();
  release_in_nested_block();
  let _ = release_after_early_return(true);
  let _ = release_after_early_return(false);
  release_after_nested_match();
  release_after_defer();
}
"#;
    compile_source(source).expect("borrow-aware control flow should compile");
}

#[test]
fn compiler_reports_live_alias() {
    let source = r#"
module tests::build;

fn main() {
  let mut int32 value = 0;
  let &mut int32 alias = &mut value;
  let &mut int32 second = &mut value;
  *second = 1;
}
"#;
    let err = compile_source(source).expect_err("expected borrow error");
    assert!(
        err.contains("already mutably borrowed"),
        "unexpected error message: {err}"
    );
}

#[test]
fn compiler_supports_mutable_destructuring() {
    let source = r#"
module tests::build_patterns;

struct Telemetry {
  hp: int32;
  mp: int32;
  notes: []string;
}

fn main() {
  let mut (left, right) = (10, 5);
  left = left + right;

  let mut #{ "hp": hp_score, "mp": mp_score } = #{
"hp": 80,
"mp": 40,
  };
  hp_score = hp_score + mp_score;

  let mut Telemetry{ hp, mp, .. } = Telemetry{
hp: 70,
mp: 35,
notes: ["alpha"],
  };
  hp = hp + mp;

  let mut [first, ..rest] = ["steady", "ready"];
  first = "launch";
  rest = rest;
}
"#;
    compile_source(source).expect("mutable destructuring should compile");
}

#[test]
fn compiler_supports_format_strings_in_out() {
    let source = r#"
module tests::format_out;

fn main() {
  let int32 hp = 15;
  out(`hp is {hp} delta {}`, hp + 5);
  out(`exact {hp}`);
  out("plain string");
}
"#;
    compile_source(source).expect("format string calls should compile");
}

#[test]
fn build_spawn_join_returns_value() {
    with_build_parallel(|| {
        let mut compiler = Compiler::new();
        let expr = Expr::Spawn {
            expr: Box::new(Expr::Literal(Literal::Int(9, Span::new(0, 0)))),
            span: Span::new(0, 0),
        };
        let handle = match compiler.emit_expression(&expr).expect("spawn evaluates") {
            EvalOutcome::Value(value) => value.into_value(),
            EvalOutcome::Flow(_) => panic!("unexpected flow from spawn"),
        };
        let joined = compiler.builtin_join(vec![handle]).expect("join returns");
        assert!(matches!(joined, Value::Int(v) if v.constant() == Some(9)));
    });
}

#[test]
fn build_spawn_join_returns_closure() {
    with_build_parallel(|| {
        let span = Span::new(0, 0);
        let param_ty = TypeAnnotation {
            ty: TypeExpr::named("int32"),
            span,
        };
        let ret_ty = TypeAnnotation {
            ty: TypeExpr::named("int32"),
            span,
        };
        let param = FunctionParam {
            name: "n".into(),
            ty: Some(param_ty.clone()),
            mutability: Mutability::Immutable,
            span,
        };
        let body_expr = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Identifier(Identifier {
                name: "n".into(),
                span,
            })),
            right: Box::new(Expr::Literal(Literal::Int(1, span))),
            span,
        };
        let closure_expr = Expr::Closure {
            params: vec![param],
            body: ClosureBody::Expr(Spanned {
                node: Box::new(body_expr),
                span,
            }),
            ret: Some(ret_ty),
            captures: Arc::new(RwLock::new(Vec::new())),
            span,
        };
        let spawn_expr = Expr::Spawn {
            expr: Box::new(closure_expr),
            span,
        };

        let mut compiler = Compiler::new();
        let handle = match compiler
            .emit_expression(&spawn_expr)
            .expect("spawn evaluates")
        {
            EvalOutcome::Value(value) => value.into_value(),
            EvalOutcome::Flow(_) => panic!("unexpected flow from spawn"),
        };
        let joined = compiler
            .builtin_join(vec![handle])
            .expect("join returns closure");
        let Value::Closure(returned) = joined else {
            panic!("expected closure from join");
        };
        let arg = EvaluatedValue::from_value(Value::Int(compiler.const_int_value(4)));
        let result = compiler
            .call_closure_value(returned, vec![arg])
            .expect("closure callable");
        match result.value() {
            Value::Int(_) => {}
            other => panic!(
                "expected int return, found {}",
                compiler.describe_value(other)
            ),
        }
    });
}

#[test]
fn build_nested_spawn_join_roundtrips() {
    with_build_parallel(|| {
        let mut compiler = Compiler::new();
        let span = Span::new(0, 0);
        let expr = Expr::Spawn {
            expr: Box::new(Expr::Spawn {
                expr: Box::new(Expr::Literal(Literal::Int(3, span))),
                span,
            }),
            span,
        };
        let outer = match compiler
            .emit_expression(&expr)
            .expect("outer spawn evaluates")
        {
            EvalOutcome::Value(value) => value.into_value(),
            EvalOutcome::Flow(_) => panic!("unexpected flow from outer spawn"),
        };
        let inner_handle = compiler
            .builtin_join(vec![outer])
            .expect("outer join returns inner handle");
        let result = compiler
            .builtin_join(vec![inner_handle])
            .expect("inner join returns value");
        assert!(matches!(result, Value::Int(v) if v.constant() == Some(3)));
    });
}

#[test]
fn build_spawn_channel_effects_bridge_to_runtime_handles() {
    let mut compiler = Compiler::new();
    let effects = vec![
        BuildEffect::ChannelCreate { id: 1 },
        BuildEffect::ChannelSend {
            id: 1,
            value: BuildValue::Int(12),
        },
        BuildEffect::ChannelClose { id: 1 },
    ];
    let channels = compiler
        .apply_build_effects(effects)
        .expect("channel effects apply");
    let inner = channels.get(&1).cloned().expect("channel present");
    let receiver = ChannelReceiver::new_with_state(inner);
    match receiver.recv() {
        Some(Value::Int(v)) => assert_eq!(v.constant(), Some(12)),
        _ => panic!("unexpected channel payload"),
    }
    assert!(receiver.recv().is_none(), "channel should be closed");
}

#[test]
fn build_spawn_channel_enums_roundtrip() {
    let mut compiler = Compiler::new();
    let effects = vec![
        BuildEffect::ChannelCreate { id: 7 },
        BuildEffect::ChannelSend {
            id: 7,
            value: BuildValue::Enum {
                enum_name: "Option".into(),
                variant: "Some".into(),
                values: vec![BuildValue::Int(21)],
                variant_index: 0,
            },
        },
        BuildEffect::ChannelClose { id: 7 },
    ];
    let channels = compiler
        .apply_build_effects(effects)
        .expect("channel effects apply");
    let rx =
        ChannelReceiver::new_with_state(channels.get(&7).cloned().expect("channel present"));
    match rx.recv() {
        Some(Value::Enum(enum_value)) => {
            assert_eq!(enum_value.enum_name, "Option");
            assert_eq!(enum_value.variant, "Some");
            assert_eq!(enum_value.variant_index, 0);
            assert!(
                matches!(enum_value.values.as_slice(), [Value::Int(v)] if v.constant() == Some(21))
            );
        }
        _ => panic!("unexpected channel payload"),
    }
    assert!(rx.recv().is_none(), "channel should be closed");
}

#[test]
fn build_spawn_join_applies_channel_effects_and_value() {
    let span = Span::new(0, 0);
    let block = Block {
        statements: vec![
            Statement::Let(LetStmt {
                pattern: Pattern::Tuple(
                    vec![
                        Pattern::Identifier("tx".into(), span),
                        Pattern::Identifier("rx".into(), span),
                    ],
                    span,
                ),
                ty: None,
                value: Some(Expr::Call {
                    callee: Box::new(Expr::Identifier(Identifier {
                        name: "channel".into(),
                        span,
                    })),
                    type_args: Vec::new(),
                    args: Vec::new(),
                    span,
                }),
                mutability: Mutability::Immutable,
                span,
            }),
            Statement::Expr(ExprStmt {
                expr: Expr::Call {
                    callee: Box::new(Expr::Identifier(Identifier {
                        name: "send".into(),
                        span,
                    })),
                    type_args: Vec::new(),
                    args: vec![
                        Expr::Identifier(Identifier {
                            name: "tx".into(),
                            span,
                        }),
                        Expr::Literal(Literal::Int(4, span)),
                    ],
                    span,
                },
            }),
            Statement::Expr(ExprStmt {
                expr: Expr::Call {
                    callee: Box::new(Expr::Identifier(Identifier {
                        name: "close".into(),
                        span,
                    })),
                    type_args: Vec::new(),
                    args: vec![Expr::Identifier(Identifier {
                        name: "tx".into(),
                        span,
                    })],
                    span,
                },
            }),
        ],
        tail: Some(Box::new(Expr::Identifier(Identifier {
            name: "rx".into(),
            span,
        }))),
        span,
    };
    let snapshot = BuildSnapshot {
        scopes: vec![BuildScope {
            bindings: HashMap::new(),
        }],
        enum_variants: HashMap::new(),
        functions: HashMap::new(),
        struct_fields: HashMap::new(),
        next_channel_id: 0,
        cleanup_stack: vec![Vec::new()],
        clock_ms: 0,
        embedded: false,
    };
    let handle = thread::spawn(move || {
        let interpreter = BuildInterpreter::new(snapshot);
        interpreter.eval_with_effects(&Expr::Block(Box::new(block)))
    });
    let join_handle = JoinHandleValue::new_build(handle);
    let mut compiler = Compiler::new();
    let joined = join_handle
        .join_with(&mut compiler)
        .expect("join applies effects");
    match joined {
        Value::Receiver(rx) => {
            match rx.recv() {
                Some(Value::Int(v)) => assert_eq!(v.constant(), Some(4)),
                _ => panic!("unexpected channel payload"),
            }
            assert!(rx.recv().is_none(), "channel should be closed");
        }
        _ => panic!("unexpected join value"),
    }
}

fn compile_entry(entry: &str) -> Result<(), String> {
    let package = load_package(Path::new(entry))
        .unwrap_or_else(|_| panic!("load package for {}", entry));
    let mut compiler = Compiler::new();
    compiler.compile_program(&package.program)
}

#[test]
fn borrow_demo_compiles() {
    compile_entry("workspace/demos/borrow/borrow_demo.prime")
        .expect("compile borrow demo in build mode");
}

#[test]
#[ignore]
fn pattern_demo_compiles() {
    // This demo is heavy and can hang build-mode under certain conditions; skip by default.
    compile_entry("workspace/demos/patterns/pattern_demo.prime")
        .expect("compile pattern demo in build mode");
}

#[test]
fn async_demo_compiles() {
    compile_entry("workspace/demos/async_demo/async_demo.prime")
        .expect("compile async demo in build mode");
}

#[test]
fn runtime_handles_cover_collections() {
    with_rt_handles(|| {
        let source = r#"
module tests::handles;

fn main() {
  let []int32 items = [1, 2];
  out(items);
  let Map[string, int32] scores = #{
"one": 1,
"two": 2,
  };
  out(scores);
  let int32 base = 5;
  let &int32 alias = &base;
  out(alias);
}
"#;
        compile_source(source)
            .expect("handles should be emitted for slices, maps, and references");
    });
}

#[test]
fn runtime_handles_cover_channels() {
    with_rt_handles(|| {
        let source = r#"
module tests::handles;

fn main() {
  let (sender, receiver) = channel();
  out(sender);
  out(receiver);
}
"#;
        compile_source(source).expect("handles should be emitted for channel endpoints");
    });
}

#[test]
fn recv_timeout_handles_closed_status() {
    with_rt_handles(|| {
        let mut compiler = Compiler::new();
        unsafe {
            let entry = llvm_sys::core::LLVMAppendBasicBlockInContext(
                compiler.context,
                compiler.main_fn,
                CString::new("rt_entry").unwrap().as_ptr(),
            );
            llvm_sys::core::LLVMPositionBuilderAtEnd(compiler.builder, entry);
        }
        compiler.ensure_runtime_symbols();
        compiler.enum_variants.insert(
            "Some".into(),
            EnumVariantInfo {
                enum_name: "Option".into(),
                fields: 1,
                module: "builtins".into(),
                visibility: Visibility::Public,
                variant_index: 0,
            },
        );
        compiler.enum_variants.insert(
            "None".into(),
            EnumVariantInfo {
                enum_name: "Option".into(),
                fields: 0,
                module: "builtins".into(),
                visibility: Visibility::Public,
                variant_index: 1,
            },
        );
        let (_, rx_handle) = compiler
            .build_channel_handles()
            .expect("channel handles available");
        let receiver = Value::Receiver(ChannelReceiver::with_handle(rx_handle));
        let millis = Value::Int(compiler.const_int_value(0));
        let result = compiler
            .builtin_recv_timeout(vec![receiver, millis])
            .expect("recv_timeout should succeed");
        match result {
            Value::Enum(enum_value) => {
                assert_eq!(enum_value.enum_name, "Option");
                assert_eq!(enum_value.variant, "None");
            }
            other => panic!(
                "expected Option::None, found {}",
                compiler.describe_value(&other)
            ),
        }
    });
}

#[test]
fn runtime_handles_cover_structs_and_pointers() {
    with_rt_handles(|| {
        let source = r#"
module tests::handles;

struct Pair { left: int32; right: int32; };

fn main() {
  let Pair value = Pair{ left: 1, right: 2 };
  let &Pair alias = &value;
  let ptr alias_ptr = ptr(alias);
  out(value);
  out(alias);
  out(alias_ptr);
}
"#;
        compile_source(source)
            .expect("handles should be emitted for structs and pointers/references");
    });
}

#[test]
fn compiler_supports_basic_closures() {
    let source = r#"
module tests::closures;

fn main() {
  let int32 base = 2;
  let add = |x: int32| base + x;
  let result = add(5);
  out(result);
}
"#;
    compile_source(source).expect("closure creation and invocation should compile");
}

#[test]
fn closure_demo_compiles() {
    compile_entry("workspace/demos/closures/closure_demo.prime")
        .expect("compile closure demo in build mode");
}

#[test]
fn closures_support_multi_return_in_build_mode() {
    let source = r#"
module tests::closures;

fn main() {
  let int32 base = 3;
  let pair = |x: int32| -> (int32, int32) { (x + base, x - base) };
  let (a, b) = pair(5);
  out(a);
  out(b);
}
"#;
    compile_source(source).expect("closure should support tuple returns in build mode");
}

#[test]
fn many_closures_allocate_and_drop_envs() {
    let source = r#"
module tests::closures;

fn main() {
  let int32 a = 1;
  let int32 b = 2;
  let int32 c = 3;
  let int32 d = 4;
  let int32 e = 5;
  let f1 = |x: int32| x + a;
  let f2 = |x: int32| x + b;
  let f3 = |x: int32| x + c;
  let f4 = |x: int32| x + d;
  let f5 = |x: int32| x + e;
  let _ = f1(1) + f2(1) + f3(1) + f4(1) + f5(1);
}
"#;
    compile_source(source).expect("many closures should allocate and free envs in build mode");
}

#[test]
fn higher_order_closure_carries_signature() {
    let source = r#"
        fn main() {
            let higher = |f: fn(int32) -> int32| -> fn(int32) -> int32 {
                |value: int32| f(value) + 1
            };
            let double = |x: int32| x * 2;
            let inc_after_double = higher(double);
            let _ = inc_after_double(4);
        }
    "#;
    let module =
        parse_module("tests::closures", PathBuf::from("higher.prime"), source).expect("parse");
    let program = Program {
        modules: vec![module],
    };
    let mut compiler = Compiler::new();
    compiler
        .compile_program(&program)
        .expect("higher-order closure should compile");
    let mut found = false;
    for info in compiler.closures.values() {
        if info.signature.params.len() == 1
            && info.signature.params[0].canonical_name() == "int32"
            && info.signature.returns.len() == 1
            && info.signature.returns[0].canonical_name() == "int32"
        {
            found = true;
            break;
        }
    }
    assert!(
        found,
        "expected captured higher-order closure to retain its (int32) -> int32 signature"
    );
}

#[test]
fn heap_handles_round_trip_in_closure_env() {
    let source = r#"
        fn main() {
            let nums = slice_new();
            slice_push(nums, 1);
            let scores = map_new();
            map_insert(scores, "a", 10);
            let counter = box_new(5);
            let pass = |_: int32| { (nums, scores, counter) };
            let _ = pass(0);
        }
    "#;
    let module = parse_module(
        "tests::closures",
        PathBuf::from("heap_handles.prime"),
        source,
    )
    .expect("parse");
    let program = Program {
        modules: vec![module],
    };
    let mut compiler = Compiler::new();
    compiler
        .compile_program(&program)
        .expect("heap handles should round-trip through closure env");
}

#[test]
fn closures_capture_references() {
    let source = r#"
module tests::closures;

fn main() {
  let int32 base = 5;
  let &int32 alias = &base;
  let reader = |_: int32| alias;
  let _ = reader(0);
}
"#;
    compile_source(source).expect("reference captures should compile");
}

#[test]
fn captured_handles_support_runtime_ops() {
    let source = r#"
module tests::closures;

fn main() {
  let []int32 nums = slice_new();
  slice_push(nums, 1);
  let Map[string, int32] scores = map_new();
  map_insert(scores, "a", 10);
  let mutate = |_: int32| {
push(nums, 2);
insert(scores, "b", 20);
let _ = get(nums, 1);
let _ = get(scores, "b");
  };
}
"#;
    compile_source(source).expect("captured handles should support mutation and read");
}

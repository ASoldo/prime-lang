Here’s a concrete design proposal for prime-lang: a small, explicit, systems language that sits between Rust, Go, and Jai:

Rust’s enums, pattern matching, safety options.

Go’s simplicity and tooling feel.

Jai’s “you are in charge, zero ceremony” vibe.

No inheritance. Composition all the way.

Explicit static typing, predictable performance, minimal runtime.

I’ll split it into a design doc + example syntax you can treat as a draft spec.

1. Core Goals

Simple mental model

No hidden magic, no implicit boxing, no “it depends” lifetimes.

You can explain the semantics on a whiteboard.

Explicit & static

All types known at compile time.

Inference allowed locally, but never ambiguous.

No runtime reflection circus.

Composition over inheritance

structs with embedding / field promotion.

Interfaces / traits as behavior, no class trees.

Zero-cost abstractions

Enums, generics, pattern matching should erase to obvious machine code.

Tooling-first

Single compiler binary.

Built-in build system: prime build, prime run.

Language server & Treesitter as first-class, not afterthoughts.

Deterministic control over memory

Rust-style “safe by default” is an option, but you can go lower-level where needed.

Clear story for stack/heap, pointers, slices.

2. Primitive Types

All explicit, no “int means whatever the platform felt like”.

Integers
int8    int16    int32    int64
uint8   uint16   uint32   uint64

isize   usize    // pointer-sized, for indexing & sizes

Floats
float32
float64

Booleans & characters
bool      // `true`, `false`
rune      // Unicode scalar value

Strings

Opinionated & simple:

string        // immutable UTF-8, length + pointer
&string       // reference to string


Later: []u8 for raw bytes.

3. Composite Types
Arrays

Fixed-size, value semantics.

let int[4] xs = [1, 2, 3, 4];

Slices

View over contiguous memory, like Go/Rust slices.

let []int nums = xs[0..2];

Structs

Core unit of data modeling. No inheritance.

struct Vec2 {
  x: float32;
  y: float32;
}

struct Transform {
  position: Vec2;
  scale: Vec2;
}

Composition (Embedding)

Field promotion by embedding:

struct Health {
  hp: int32;
  max_hp: int32;
}

struct Player {
  Transform;   // embedded
  Health;      // embedded
  name: string;
}


Usage:

let Player p = Player{
  Transform{ Vec2{0, 0}, Vec2{1, 1} },
  Health{ 100, 100 },
  "Prime Hero",
};

p.hp = 50;          // from embedded Health
p.position.x = 10;  // from embedded Transform


No subclassing, no vtables. Just layout + sugar.

4. Enums & Algebraic Data Types

Rust-style sum types, no excuses.

enum Result[T, E] {
  Ok(T),
  Err(E),
}

enum Expr {
  Int(int64),
  Add(Expr, Expr),
  Sub(Expr, Expr),
}


Enough to support pattern matching properly.

5. Pattern Matching

Ergonomic, exhaustive by default.

fn eval(e: Expr) -> int64 {
  match e {
    Int(v)          => v,
    Add(lhs, rhs)   => eval(lhs) + eval(rhs),
    Sub(lhs, rhs)   => eval(lhs) - eval(rhs),
  }
}


On Result:

fn use_file(r: Result[int32, string]) -> int32 {
  match r {
    Ok(v)    => v,
    Err(msg) => {
      out("error: ");
      out(msg);
      0
    },
  }
}


Later: guards.

6. Functions
Basic
fn add(a: int32, b: int32) -> int32 {
  a + b
}


Allow expression body syntax when single return:

fn square(x: int32) -> int32 => x * x;

Multiple Return

Go-style, extremely useful & simple.

fn divmod(a: int32, b: int32) -> (int32, int32) {
  let int q = a / b;
  let int r = a % b;
  (q, r)
}

First-class functions (later)

Minimal but predictable:

fn map(xs: []int32, f: fn(int32) -> int32) {
  // ...
}

7. Variables & Assignment

Right now you use let int a = 6;. Expand that generically:

let int32 x = 10;
let x = 10;          // allowed with local inference


Reassignment:

let int32 a = 1;
a = 2;


Const:

const int32 N = 64;


Rule: let is mutable by default, or flip that if you want Rust brain. Just pick one and never be cute about it.

8. References & Pointers

Keep it explicit. No implicit borrowing magic unless you truly earn it.

Syntax suggestion:

let int32 value = 42;
let &int32 ref_to_value = &value;
let *int32 ptr = alloc_int32(42);  // raw pointer, manual free


Suggested semantics:

&T:

Non-owning reference, safe, cannot outlive owner (enforced by simple lifetime rules or region-based).

*T:

Owning / unsafe pointer. You can do bad things; compiler shrugs.

Don’t over-Rust it at v1. Start with:

no aliasing rules,

but clear safe vs unsafe blocks later.

9. Control Flow

You don’t have while. You’re suffering. Add the basics.

if cond {
  ...
} else {
  ...
}

for i in 0..10 {
  ...
}

while cond {
  ...
}


Or go full Jai-style for and skip while. Design choice.

Also:

return expr;
break;
continue;


Predictable, nothing fancy.

10. Error Handling

Two strong options. Use both:

Multi-return:

fn read_config(path: string) -> (Config, bool) {
  // ok, ok?
}


Result enum builtin:

fn read_config(path: string) -> Result[Config, Error] { ... }

match read_config("cfg") {
  Ok(cfg) => use(cfg),
  Err(e)  => out(e.message),
}


You can later sugar this:

let cfg = try read_config("cfg");


But don’t start with macros & syntax soup.

11. Modules & Imports

Jai-style: simple, file & folder based. No cursed module resolution.

// math.prime
fn add(a: int32, b: int32) -> int32 {
  a + b
}

// main.prime
import "math";

fn main() {
  let int32 x = math.add(2, 3);
  out(x);
}


Optionally:

using math;  // bring symbols into scope

12. Build & Tooling

Required for seriousness:

prime build:

Single-step compile.

prime run main.prime.

prime fmt:

Stable code formatter.

prime lsp:

Built-in LSP mode (or flag).

No JSON-based config hydra. One prime.toml or similar.

13. Example: prime-lang “real” sample

Tie it all:

// types.prime
struct Vec2 {
  x: float32;
  y: float32;
}

struct Player {
  Vec2;           // embedded position
  name: string;
  hp: int32;
}

enum Damage {
  Flat(int32),
  Percent(float32),
}

// main.prime
import "types";

fn apply_damage(p: Player, d: Damage) -> Player {
  match d {
    Flat(amount)      => Player{ p.x, p.y, p.name, p.hp - amount },
    Percent(percent)  => Player{ p.x, p.y, p.name, p.hp - (p.hp * percent) },
  }
}

fn main() {
  let Player hero = Player{ 0.0, 0.0, "Prime", 100 };

  let Player hit1 = apply_damage(hero, Flat(10));
  let Player hit2 = apply_damage(hit1, Percent(0.25));

  out(hit2.hp);
}


Clean, explicit, no “class”, no GC ceremony, compiles straight to LLVM.

You now have a coherent first-pass spec:

strong static types,

enums + match,

composition,

value semantics,

a sane path to low-level control.

Here’s what I’d lock in for prime-lang so it’s simple, sharp, and doesn’t degenerate into cosplay-Rust.

1. Mutability Rules

Immutability by default. Explicit mut. Non-negotiable.

It buys safety, clarity, and optimizer friendliness without extra ceremony.

Proposal
let x = 10;          // immutable
let mut y = 20;      // mutable
y = 30;              // ok
x = 40;              // compile error


Applies to:

Locals

Struct fields:

Mutability of the binding, not per-field magic.

Function params:

fn bump(mut x: int32) -> int32 {
  x = x + 1;
  x
}


Why:
Easy mental model, aligns with systems dev expectations, no “surprise mutation from 8 scopes away.”

2. Ownership Model

This is where languages either become elegant or insufferable.

You said “between Rust and Go” & “Jai-level usability,” so:

Pick: value semantics + simple references + explicit unsafe, NOT full Rust borrow-checker v1.

Base rules

Value semantics by default

struct Vec2 { x: float32; y: float32; }

fn move_player(mut p: Vec2) {
  p.x = p.x + 1.0;
}


Call by value copies. Large structs can be optimized by the compiler.

References for aliasing

Introduce &T as “borrowed view, non-owning”:

fn length(v: &Vec2) -> float32 {
  // read-only if `v` is not `mut`
  (v.x * v.x + v.y * v.y)
}

fn translate(mut v: &Vec2, dx: float32, dy: float32) {
  v.x = v.x + dx;
  v.y = v.y + dy;
}


Enforce basic rules:

&T cannot outlive the variable it references (simple region-based / lexical check).

No aliasing rules gymnastics initially. Just ban obviously dangling references.

Raw pointers for maniacs

fn poke(p: *int32) {
  unsafe {
    *p = 13;
  }
}


*T:

No guarantees.

Only usable in unsafe {}.

You do this when you talk to C, drivers, allocators, etc.

Ownership of heap objects

Phase 1 (keep it boring and workable):

Provide standard library helpers like:

struct Box[T] {
  ptr: *T;
}

fn box_new[T](value: T) -> Box[T];
fn box_free[T](b: Box[T]);


User knows: Box owns. Compiler can later enforce move semantics for these without going full Rust.

Roadmap (not v1):

Add move semantics for certain types (Box, Array, etc.).

Add defer and scoped cleanups.

Maybe region-based ownership to keep it tractable.

But do not ship v1 with a half-baked Rust borrow checker. You’ll sink months into pain for negative user value.

3. Sugar: What’s Allowed & What’s Banned

You want “simple but powerful,” not “15 ways to write hello world.”

Allowed (you should do these)

Local type inference

let x = 10;          // infers int32
let mut v = Vec2{1.0, 2.0};


Named & positional struct init

let p = Player{
  name: "Hero",
  hp: 100,
};

let v = Vec2{ 1.0, 2.0 };


Method-style call sugar (optional but nice)

Desugar only:

fn length(v: &Vec2) -> float32 { ... }

let l = v.length();  // sugar for length(&v)


No traits required for v1. Just symbol resolution.

Pattern matching on enums

You already want this. Keep it.

match res {
  Ok(v)    => out(v),
  Err(msg) => out(msg),
}


defer

You’re systems-leaning. Add:

fn main() {
  let fh = open("file");
  defer close(fh);

  // ...
}


Single keyword, huge practical win.

Banned (for now, to keep it sane)

No operator overloading
You are not C++. You want predictable codegen.

No macros / templates from hell
If you add meta, make it:

hygienic,

minimal,

compile-time only,

readable.

But not v1. Get the core language correct first.

No implicit conversions across numeric types

int32 -> int64  // must be explicit cast
float32 -> float64 // explicit


This prevents half of “mysterious bugs in production.”

No inheritance. Ever.
Structs + interfaces/traits later if needed. That’s it.

4. Concrete snapshot of prime-lang v1

If I had to freeze a v1 spec from what we’ve discussed:

Types

bool, rune

int8/16/32/64, uint8/16/32/64, isize, usize

float32, float64

string

arrays [N]T, slices []T

struct, enum, Result[T,E] in std

Core features

fn with explicit types

multiple return values

match on enums, exhaustive

if, for, while (or single powerful loop form)

let with mut

&T references, *T unsafe pointers

defer

import by path: import "foo/bar"

Philosophy

Explicit, tight, easy to implement.

Can be compiled in one pass with some constraints.

Feels like: “If Rust and Go had a child supervised by someone who actually reads assembly.”

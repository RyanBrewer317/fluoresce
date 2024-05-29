// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import fluoresce.{
  type Cont, type Not, type Subtract, Cont, apply, bind, co, compose, throw,
  wrap,
}

pub fn disjunctive_syllogism(
  e: Cont(r, Result(b, a)),
  not_a: Not(r, a),
) -> Cont(r, b) {
  apply(e, not_a)
}

pub fn excluded_middle() -> Cont(r, Result(Not(r, a), a)) {
  use cofn <- co(fn(k) { wrap(k) })
  wrap(cofn)
}

pub fn double_negate(a: a) -> Cont(r, Not(r, Not(r, a))) {
  use lem <- bind(excluded_middle())
  case lem {
    Error(not_a) -> throw(a, not_a)
    Ok(not_not_a) -> wrap(not_not_a)
  }
}

pub fn adjunction(
  f: fn(Not(r, a)) -> Cont(r, b),
) -> Cont(r, fn(Not(r, b)) -> Cont(r, a)) {
  use s <- co(fn(k) { f(k) })
  let t =
    wrap(case s {
      Error(a) -> Ok(a)
      Ok(b) -> Error(b)
    })
  wrap(fn(not_b) { disjunctive_syllogism(t, not_b) })
}

pub fn double_negation_elimination(not_not_a: Not(r, Not(r, a))) -> Cont(r, a) {
  use f <- bind(adjunction(fn(not_a) { wrap(not_a) }))
  f(not_not_a)
}

pub fn contramap(not_b: Not(r, b), f: fn(a) -> Cont(r, b)) -> Cont(r, Not(r, a)) {
  use f2 <- bind(
    adjunction(fn(not_not_a) {
      use a <- bind(double_negation_elimination(not_not_a))
      f(a)
    }),
  )
  f2(not_b)
}

pub fn implies(f: fn(a) -> Cont(r, b)) -> Cont(r, Result(b, Not(r, a))) {
  use s <- co(fn(not_not_a) { bind(double_negation_elimination(not_not_a), f) })
  wrap(s)
}

pub fn modus_ponens(f: Result(b, Not(r, a)), a) -> Cont(r, b) {
  use not_not_a <- bind(double_negate(a))
  disjunctive_syllogism(wrap(f), not_not_a)
}

pub fn de_morgan_1(x: Not(r, Result(b, a))) -> Cont(r, #(Not(r, a), Not(r, b))) {
  use l <- bind(contramap(x, compose(wrap, Error)))
  use r <- bind(contramap(x, compose(wrap, Ok)))
  wrap(#(l, r))
}

pub fn de_morgan_2(x: #(Not(r, a), Not(r, b))) -> Cont(r, Not(r, Result(b, a))) {
  let #(not_a, not_b) = x
  contramap(not_b, fn(a) { disjunctive_syllogism(wrap(a), not_a) })
}

pub fn de_morgan_3(x: Not(r, #(a, b))) -> Cont(r, Result(Not(r, b), Not(r, a))) {
  use f <- bind(
    adjunction(fn(y) {
      use #(not_not_a, not_not_b) <- bind(de_morgan_1(y))
      use a <- bind(double_negation_elimination(not_not_a))
      use b <- bind(double_negation_elimination(not_not_b))
      wrap(#(a, b))
    }),
  )
  f(x)
}

pub fn de_morgan_4(x: Result(Not(r, b), Not(r, a))) -> Cont(r, Not(r, #(a, b))) {
  use lem <- bind(excluded_middle())
  case lem {
    Error(#(a, b)) ->
      case x {
        Error(not_a) -> throw(a, not_a)
        Ok(not_b) -> throw(b, not_b)
      }
    Ok(not_pair) -> wrap(not_pair)
  }
}

pub fn function_to_coexponential(
  f: fn(a) -> Cont(r, b),
) -> Cont(r, Not(r, Subtract(r, a, b))) {
  use or <- bind(implies(f))
  case or {
    Error(not_a) -> de_morgan_4(Error(not_a))
    Ok(b) -> {
      use not_not_b <- bind(double_negate(b))
      de_morgan_4(Ok(not_not_b))
    }
  }
}

pub fn coexponential_to_function(
  c: Not(r, Subtract(r, a, b)),
) -> Cont(r, fn(a) -> Cont(r, b)) {
  fn(a) {
    use or <- bind(de_morgan_3(c))
    case or {
      Error(not_a) -> throw(a, not_a)
      Ok(not_not_b) -> double_negation_elimination(not_not_b)
    }
  }
  |> wrap
}

pub fn pierces_law(f: fn(fn(a) -> Cont(r, b)) -> Cont(r, a)) -> Cont(r, a) {
  use or <- bind(implies(f))
  case or {
    Error(not_a) -> {
      use f <- bind(adjunction(coexponential_to_function))
      use #(out, _) <- bind(f(not_a))
      wrap(out)
    }
    Ok(a) -> wrap(a)
  }
}

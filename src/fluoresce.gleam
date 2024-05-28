// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/io

fn compose(f: fn(b) -> c, g: fn(a) -> b) -> fn(a) -> c {
  fn(x) { f(g(x)) }
}

pub type Not(r, a) =
  fn(a) -> r

pub type NotNot(r, a) {
  Cont(run: fn(fn(a) -> r) -> r)
}

pub type Subtract(r, a, b) =
  #(a, Not(r, b))

pub fn wrap(a: a) -> NotNot(r, a) {
  Cont(fn(k) { k(a) })
}

pub fn do(c: NotNot(r, a), f: fn(a) -> b) -> NotNot(r, b) {
  Cont(fn(k) { c.run(fn(a) { k(f(a)) }) })
}

pub fn bind(c: NotNot(r, a), f: fn(a) -> NotNot(r, b)) -> NotNot(r, b) {
  Cont(fn(k) { c.run(fn(a) { f(a).run(k) }) })
}

pub fn join(a: NotNot(r, NotNot(r, a))) -> NotNot(r, a) {
  Cont(fn(k) { a.run(fn(b) { b.run(k) }) })
}

pub fn co(
  f: fn(Not(r, a)) -> NotNot(r, b),
  rest: fn(Result(b, a)) -> NotNot(r, c),
) -> NotNot(r, c) {
  Cont(fn(k) {
    let k2 = fn(a) { rest(a).run(k) }
    f(compose(k2, Error)).run(compose(k2, Ok))
  })
}

pub fn disjunctive_syllogism(
  e: NotNot(r, Result(b, a)),
  not_a: Not(r, a),
) -> NotNot(r, b) {
  Cont(fn(k) {
    e.run(fn(res) {
      case res {
        Error(a) -> not_a(a)
        Ok(e) -> k(e)
      }
    })
  })
}

pub fn callcc(f: fn(Not(r, a)) -> a) -> NotNot(r, a) {
  use s <- co(fn(k) { wrap(f(k)) })
  case s {
    Error(a) -> a
    Ok(a) -> a
  }
  |> wrap
}

pub fn throw(a: a, not_a: Not(r, a)) -> NotNot(r, b) {
  let instant_fail = wrap(Error(a))
  disjunctive_syllogism(instant_fail, not_a)
}

pub fn excluded_middle() -> NotNot(r, Result(Not(r, a), a)) {
  use cofn <- co(fn(k) { wrap(k) })
  wrap(cofn)
}

pub fn double_negate(a: a) -> NotNot(r, Not(r, Not(r, a))) {
  use lem <- bind(excluded_middle())
  case lem {
    Error(not_a) -> throw(a, not_a)
    Ok(not_not_a) -> wrap(not_not_a)
  }
}

pub fn adjunction(
  f: fn(Not(r, a)) -> NotNot(r, b),
) -> NotNot(r, fn(Not(r, b)) -> NotNot(r, a)) {
  use s <- co(fn(k) { f(k) })
  let t =
    wrap(case s {
      Error(a) -> Ok(a)
      Ok(b) -> Error(b)
    })
  wrap(fn(not_b) { disjunctive_syllogism(t, not_b) })
}

pub fn double_negation_elimination(not_not_a: Not(r, Not(r, a))) -> NotNot(r, a) {
  use f <- bind(adjunction(fn(not_a) { wrap(not_a) }))
  f(not_not_a)
}

pub fn contramap(
  not_b: Not(r, b),
  f: fn(a) -> NotNot(r, b),
) -> NotNot(r, Not(r, a)) {
  use f2 <- bind(
    adjunction(fn(not_not_a) {
      use a <- bind(double_negation_elimination(not_not_a))
      f(a)
    }),
  )
  f2(not_b)
}

pub fn implies(f: fn(a) -> NotNot(r, b)) -> NotNot(r, Result(b, Not(r, a))) {
  use s <- co(fn(not_not_a) { bind(double_negation_elimination(not_not_a), f) })
  wrap(s)
}

pub fn modus_ponens(f: Result(b, Not(r, a)), a) -> NotNot(r, b) {
  use not_not_a <- bind(double_negate(a))
  disjunctive_syllogism(wrap(f), not_not_a)
}

pub fn de_morgan_1(x: Not(r, Result(b, a))) -> NotNot(r, #(Not(r, a), Not(r, b))) {
  use l <- bind(contramap(x, compose(wrap, Error)))
  use r <- bind(contramap(x, compose(wrap, Ok)))
  wrap(#(l, r))
}

pub fn de_morgan_2(x: #(Not(r, a), Not(r, b))) -> NotNot(r, Not(r, Result(b, a))) {
  let #(not_a, not_b) = x
  contramap(not_b, fn(a) { disjunctive_syllogism(wrap(a), not_a) })
}

pub fn de_morgan_3(x: Not(r, #(a, b))) -> NotNot(r, Result(Not(r, b), Not(r, a))) {
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

pub fn de_morgan_4(x: Result(Not(r, b), Not(r, a))) -> NotNot(r, Not(r, #(a, b))) {
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
  f: fn(a) -> NotNot(r, b),
) -> NotNot(r, Not(r, Subtract(r, a, b))) {
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
) -> NotNot(r, fn(a) -> NotNot(r, b)) {
  fn(a) {
    use or <- bind(de_morgan_3(c))
    case or {
      Error(not_a) -> throw(a, not_a)
      Ok(not_not_b) -> double_negation_elimination(not_not_b)
    }
  }
  |> wrap
}

pub fn pierces_law(f: fn(fn(a) -> NotNot(r, b)) -> NotNot(r, a)) -> NotNot(r, a) {
  use or <- bind(implies(f))
  case or {
    Error(not_a) -> {
      use f <- bind(adjunction(coexponential_to_function))
      use #(out, _) <- do(f(not_a))
      out
    }
    Ok(a) -> wrap(a)
  }
}

pub fn main() {
  io.println("Hello from fluoresce!")
  let g = {
    use not_not_a <- bind(double_negate(3))
    use _x <- bind(callcc(fn(_k) { 
      io.debug("hi")
    }))
    use lem <- do(excluded_middle())
    case lem {
      Error(not_a) -> throw(not_a, not_not_a)
      Ok(_) -> double_negation_elimination(not_not_a)
    }
  }
  g.run(fn(a) { a.run(io.debug) })
}

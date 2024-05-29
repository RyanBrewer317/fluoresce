// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

@internal
pub fn compose(f: fn(b) -> c, g: fn(a) -> b) -> fn(a) -> c {
  fn(x) { f(g(x)) }
}

pub type Not(r, a) =
  fn(a) -> r

pub type Cont(r, a) {
  Cont(run: fn(fn(a) -> r) -> r)
}

pub type Subtract(r, a, b) =
  #(a, Not(r, b))

pub fn wrap(a: a) -> Cont(r, a) {
  Cont(fn(k) { k(a) })
}

pub fn bind(c: Cont(r, a), f: fn(a) -> Cont(r, b)) -> Cont(r, b) {
  Cont(fn(k) { c.run(fn(a) { f(a).run(k) }) })
}

pub fn join(a: Cont(r, Cont(r, a))) -> Cont(r, a) {
  Cont(fn(k) { a.run(fn(b) { b.run(k) }) })
}

pub fn co(
  f: fn(Not(r, a)) -> Cont(r, b),
  rest: fn(Result(b, a)) -> Cont(r, c),
) -> Cont(r, c) {
  Cont(fn(k) {
    let k2 = fn(a) { rest(a).run(k) }
    f(compose(k2, Error)).run(compose(k2, Ok))
  })
}

pub fn callcc(f: fn(Not(r, a)) -> a) -> Cont(r, a) {
  use s <- co(fn(k) { wrap(f(k)) })
  case s {
    Error(a) -> a
    Ok(a) -> a
  }
  |> wrap
}

pub fn apply(e: Cont(r, Result(b, a)), not_a: Not(r, a)) -> Cont(r, b) {
  Cont(fn(k) {
    e.run(fn(res) {
      case res {
        Error(a) -> not_a(a)
        Ok(e) -> k(e)
      }
    })
  })
}

pub fn throw(a: a, not_a: Not(r, a)) -> Cont(r, b) {
  let instant_fail = wrap(Error(a))
  apply(instant_fail, not_a)
}

import gleeunit
import gleeunit/should
import fluoresce.{wrap, bind, callcc}

pub fn main() {
  gleeunit.main()
}

pub fn hello_world_test() {
  {
    use a <- bind(wrap(1))
    use b <- bind(wrap(2))
    use c <- bind(wrap(b - a))
    wrap(c)
  }.run(fn(a) { a })
  |> should.equal(1)
}

pub fn callcc_test() {
  {
    use a <- bind(wrap(1))
    // the rest of the program has a hole `b` of type Int
    // so we can think of it as a function Int->t for some t
    // `callcc` makes it actually a function, which we call k
    // the rest of the code is executed, filling the hole with 3
    // so this is the same as just setting b to 3
    // the result of all that is then 2, so code finally progresses with b as 2
    use b <- bind(callcc(fn(k) { k(3) }))
    wrap(b - a)
  }.run(fn(a) { a })
  |> should.equal(1)
}
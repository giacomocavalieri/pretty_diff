import birdie
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleeunit
import gleeunit/should
import pretty_diff

pub fn main() {
  gleeunit.main()
}

pub fn diff_of_dynamics_test() {
  check_equal_diff(dynamic_from(1))

  pretty_diff.diff(dynamic_from(1), dynamic_from("string"))
  |> should.equal(pretty_diff.DifferentTypes(
    dynamic_from(1),
    dynamic_from("string"),
  ))
}

pub fn diff_of_ints_test() {
  check_equal_diff(1)

  pretty_diff.diff(1, 2) |> should.equal(pretty_diff.Ints(1, 2))
}

pub fn diff_of_float_test() {
  check_equal_diff(1.1)

  pretty_diff.diff(1.1, 2.1) |> should.equal(pretty_diff.Floats(1.1, 2.1))
  pretty_diff.diff(1.0, 2.1) |> should.equal(pretty_diff.Floats(1.0, 2.1))
  pretty_diff.diff(1.1, 2.0) |> should.equal(pretty_diff.Floats(1.1, 2.0))
}

@target(erlang)
pub fn diff_of_float_erlang_test() {
  pretty_diff.diff(1.0, 2.0) |> should.equal(pretty_diff.Floats(1.0, 2.0))
}

@target(javascript)
pub fn diff_of_float_javascript_test() {
  pretty_diff.diff(1.0, 2.0) |> should.equal(pretty_diff.Ints(1, 2))
}

pub fn diff_of_bool_test() {
  check_equal_diff(True)
  check_equal_diff(False)

  pretty_diff.diff(True, False) |> should.equal(pretty_diff.Bools(True, False))
  pretty_diff.diff(False, True) |> should.equal(pretty_diff.Bools(False, True))
}

pub fn diff_of_string_test() {
  check_equal_diff("")
  check_equal_diff("aaa")

  pretty_diff.diff("a", "b") |> should.equal(pretty_diff.Strings("a", "b"))
}

pub fn diff_of_bit_arrays_test() {
  check_equal_diff(<<>>)
  check_equal_diff(<<123>>)
}

@target(erlang)
pub fn diff_of_bit_arrays_erlang_test() {
  pretty_diff.diff(<<1:1>>, <<0:1>>)
  |> should.equal(pretty_diff.BitArrays(<<1:1>>, <<0:1>>))

  pretty_diff.diff(<<65>>, <<66>>)
  |> should.equal(pretty_diff.Strings("A", "B"))
}

@target(javascript)
pub fn diff_of_bit_arrays_erlang_test() {
  pretty_diff.diff(<<65>>, <<66>>)
  |> should.equal(pretty_diff.BitArrays(<<65>>, <<66>>))
}

pub fn diff_of_lists_test() {
  check_equal_diff([])
  check_equal_diff([1, 2, 3])

  pretty_diff.diff(["a", "b", "c"], ["a", "a", "c"])
  |> should.equal(
    pretty_diff.Lists([
      pretty_diff.Shared(1, pretty_diff.Equal(dynamic_from("a"))),
      pretty_diff.Shared(2, pretty_diff.Strings("b", "a")),
      pretty_diff.Shared(3, pretty_diff.Equal(dynamic_from("c"))),
    ]),
  )

  pretty_diff.diff(between: [1, 2, 3], and: [0, 2, 3, 4])
  |> should.equal(
    pretty_diff.Lists([
      pretty_diff.Shared(1, pretty_diff.Ints(1, 0)),
      pretty_diff.Shared(2, pretty_diff.Equal(dynamic_from(2))),
      pretty_diff.Shared(3, pretty_diff.Equal(dynamic_from(3))),
      pretty_diff.Right(4, dynamic_from(4)),
    ]),
  )
}

pub fn diff_of_tuples_test() {
  check_equal_diff(#())
  check_equal_diff(#(1, 2, 3))

  pretty_diff.diff(between: #(1, 2, 3), and: #(1, 3, 2))
  |> should.equal(
    pretty_diff.Tuples([
      pretty_diff.Shared(1, pretty_diff.Equal(dynamic_from(1))),
      pretty_diff.Shared(2, pretty_diff.Ints(2, 3)),
      pretty_diff.Shared(3, pretty_diff.Ints(3, 2)),
    ]),
  )
}

pub fn diff_of_dicts_test() {
  check_equal_diff(dict.new())
  check_equal_diff(dict.from_list([#(1, "a"), #(2, "b")]))

  pretty_diff.diff(
    dict.from_list([#(1, "a"), #(2, "b"), #(3, "d")]),
    dict.from_list([#(1, "a"), #(2, "c"), #(4, "e")]),
  )
  |> should.equal(pretty_diff.Dicts(
    only_left: dict.from_list([#(dynamic_from(3), dynamic_from("d"))]),
    only_right: dict.from_list([#(dynamic_from(4), dynamic_from("e"))]),
    shared: dict.from_list([
      #(dynamic_from(1), pretty_diff.Equal(dynamic_from("a"))),
      #(dynamic_from(2), pretty_diff.Strings("b", "c")),
    ]),
  ))
}

pub type Wibble {
  Wibble
  Wobble(Int, String)
}

pub fn diff_of_custom_types_test() {
  check_equal_diff(Wibble)
  check_equal_diff(Wobble(1, "a"))

  pretty_diff.diff(Wibble, Wobble(1, "a"))
  |> should.equal(
    pretty_diff.CustomTypes(pretty_diff.DifferentConstructors(
      dynamic_from(Wibble),
      dynamic_from(Wobble(1, "a")),
    )),
  )

  pretty_diff.diff(Wobble(1, "a"), Wibble)
  |> should.equal(
    pretty_diff.CustomTypes(pretty_diff.DifferentConstructors(
      dynamic_from(Wobble(1, "a")),
      dynamic_from(Wibble),
    )),
  )

  pretty_diff.diff(Wobble(1, "a"), Wobble(1, "b"))
  |> should.equal(
    pretty_diff.CustomTypes(
      pretty_diff.SameConstructors("Wobble", [
        pretty_diff.Equal(dynamic_from(1)),
        pretty_diff.Strings("a", "b"),
      ]),
    ),
  )
}

type Level {
  A1
  C1
  C2
  Native
}

type User {
  User(
    name: String,
    age: Int,
    hobbies: List(String),
    speaks: Dict(String, Level),
  )
}

pub fn to_string_of_complex_type_1_test() {
  let one =
    User(
      "Giacomo",
      25,
      ["FP", "Gleam", "programming"],
      dict.from_list([#("Italian", Native), #("English", C2), #("Spanish", A1)]),
    )

  let other =
    User(
      "Tommaso",
      25,
      ["OOP", "Java", "programming", "playing the piano"],
      dict.from_list([#("Italian", Native), #("English", C1), #("Japanese", C1)]),
    )

  birdie.snap(pretty_diff.from(one, and: other), title: "complex diff 1")
}

fn check_equal_diff(value: a) {
  pretty_diff.diff(value, value)
  |> should.equal(pretty_diff.Equal(dynamic_from(value)))
}

@external(erlang, "gleam@function", "identity")
@external(javascript, "../gleam_stdlib/gleam/function.mjs", "identity")
fn dynamic_from(value: a) -> Dynamic

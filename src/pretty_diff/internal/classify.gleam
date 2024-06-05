import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}
import gleam/result
import gleam/string

// TYPES -----------------------------------------------------------------------

pub type TypedValue {
  /// The result you get from classifying a `String`.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// classify(dynamic.from("Hello!"))
  /// // -> StringValue("Hello!")
  /// ```
  ///
  StringValue(String)

  /// The result you get from classifying an `Int`.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// classify(dynamic.from(1))
  /// // -> IntValue(1)
  /// ```
  ///
  IntValue(Int)

  /// The result you get from classifying a `Float`.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// classify(dynamic.from(1.1))
  /// // -> FloatValue(1.1)
  /// ```
  ///
  FloatValue(Float)

  /// The result you get from classifying a `Bool`.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// classify(dynamic.from(True))
  /// // -> BoolValue(True)
  /// ```
  ///
  BoolValue(Bool)

  /// The result you get from classifying `Nil`.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// classify(dynamic.from(Nil))
  /// // -> NilValue
  /// ```
  ///
  NilValue

  /// The result you get from classifying a `BitArray`.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// classify(dynamic.from(<<0:1, 1:1>>))
  /// // -> BitArrayValue(<<0:1, 1:1>>)
  /// ```
  ///
  BitArrayValue(BitArray)

  /// The result you get from classifying a `List`.
  /// Its items won't be classified but left as dynamics.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// classify(dynamic.from([1, 2]))
  /// // -> ListValue([dynamic.from(1), dynamic.from(2)])
  /// ```
  ///
  ListValue(items: List(Dynamic))

  /// The result you get from classifying a `Dict`.
  /// Its keys and values won't be classified but left as dynamics.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// dict.from_list([#(1, "a"), #(2, "b")])
  /// |> dynamic.from
  /// |> classify
  /// // -> DictValue(dict.from_list([
  /// //   #(dynamic.from(1), dynamic.from("a")),
  /// //   #(dynamic.from(2), dynamic.from("b")),
  /// // ])
  /// ```
  ///
  DictValue(items: Dict(Dynamic, Dynamic))

  /// The result you get from classifying a `Tuple`.
  /// Its items won't be classified but left as dynamics.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// classify(dynamic.from(#(1, 2)))
  /// // -> TupleValue([dynamic.from(1), dynamic.from(2)])
  /// ```
  ///
  TupleValue(items: List(Dynamic))

  /// The result you get from classifying the variant of a custom type.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// classify(dynamic.from(Ok(1)))
  /// // -> CustomValue("Ok", dynamic.from(1))
  ///
  /// classify(dynamic.from(Error("uh oh")))
  /// // -> CustomValue("Error", dynamic.from("uh oh"))
  /// ```
  ///
  CustomValue(name: String, fields: List(Field))

  /// The result from trying to classify a function or a JavaScript object.
  /// Since there's no way to type-safely classify them, they will just be
  /// turned into a string.
  ///
  ForeignValue(String)
}

/// A custom value's field. It might be labelled and holds a dynamic value.
///
pub type Field {
  Field(label: Option(String), value: Dynamic)
}

// DECODERS --------------------------------------------------------------------

/// Classifies a `Dynamic` value back into a typed value.
///
/// ## Examples
///
/// ```gleam
/// classify(dynamic.from(1))
/// // -> IntValue(1)
/// ```
///
/// ```gleam
/// classify(dynamic.from("Hello!"))
/// // -> StringValue("Hello!")
/// ```
///
/// ⚠️ It's not always possible to retrieve the exact same value from
/// a dynamic because some types might have the same runtime representation
/// depending on the target.
///
/// ### Erlang quirks
///
/// - On the Erlang target `String`s and `BitArray`s have the same runtime
///   representation so if you try to classify a `BitArray` that is also a valid
///   utf8 `String` you'll get a `StringValue` back:
///
///   ```gleam
///   classify(<<65, 66>>)
///   // -> StringValue("AB")
///   ```
///
/// - On the Erlang target custom types are repesented as tagged tuples, so
///   there's no way to tell apart a tuple like `#(Wibble, 1)` and `Wibble(1)`.
///   Any tuple with a no-fields variant as it's first item will be interpreted
///   as a `CustomValue`:
///
///   ```gleam
///   pub type Wibble { Wibble }
///
///   classify(dynamic.from(#(Wibble, 1)))
///   // -> CustomValue("Wibble", [Field(None, dynamic.from(1))])
///   ```
///
/// - On the Erlang target there's no information on field labels at runtime so,
///   even if a constructor specifies label names, a `CustomValue`'s field will
///   never have a label:
///
///   ```gleam
///   pub type Wibble { Wibble(label: Int) }
///
///   classify(dynamic.from(Wibble(1)))
///   // -> CustomValue("Wibble", [Field(None, dynamic.from(1))])
///   ```
///
/// ### JavaScript quirks
///
/// - On the JavaScript target both `Int`s and `Float`s are represented as plain
///   js numbers. So if you try and classify a floating point number that looks
///   like an integer you'll get an `IntValue` back:
///
///   ```gleam
///   classify(dynamic.from(1.0))
///   // -> IntValue(1)
///   ```
///
/// - If you're minifying your JavaScript compiled code then constructors' names
///   (and field labels) could be changed by the minifier, resulting in
///   unexpected results when inspecting a `CustomValue`:
///
///   ```gleam
///   classify(dynamic.from(Ok(1)))
///   // -> CustomValue("Ok", [Field(None, dynamic.from(1))])
///   //                ^^^^ You can't rely on this always being called `Ok`
///   //                     because the minifier might change that name!!
///   ```
///
pub fn classify(value: Dynamic) -> TypedValue {
  let assert Ok(t) = decode_type(value)
  t
}

/// This decoder will always be `Ok`. It returns `Result` so that it is
/// compatible with other decoders.
///
fn decode_type(value: Dynamic) -> Result(TypedValue, List(dynamic.DecodeError)) {
  use <- result.lazy_or(result.map(dynamic.int(value), IntValue))
  use <- result.lazy_or(result.map(dynamic.float(value), FloatValue))
  use <- result.lazy_or(result.map(dynamic.string(value), StringValue))
  use <- result.lazy_or(result.map(dynamic.bool(value), BoolValue))
  use <- result.lazy_or(result.map(decode_nil(value), fn(_) { NilValue }))
  use <- result.lazy_or(result.map(dynamic.bit_array(value), BitArrayValue))
  use <- result.lazy_or(decode_custom_type(value))
  use <- result.lazy_or(result.map(decode_tuple(value), TupleValue))
  use <- result.lazy_or(result.map(dynamic.shallow_list(value), ListValue))
  use <- result.lazy_or(result.map(
    dynamic.dict(dynamic.dynamic, dynamic.dynamic)(value),
    DictValue,
  ))

  // Anything else we just inspect. This could be a function or an external
  // object or type from the runtime.
  Ok(ForeignValue(string.inspect(value)))
}

// FFI -------------------------------------------------------------------------

@external(erlang, "pretty_diff_classify_ffi", "decode_custom_type")
@external(javascript, "../../pretty_diff_classify_ffi.mjs", "decode_custom_type")
fn decode_custom_type(
  value: Dynamic,
) -> Result(TypedValue, List(dynamic.DecodeError))

@external(erlang, "pretty_diff_classify_ffi", "decode_tuple")
@external(javascript, "../../pretty_diff_classify_ffi.mjs", "decode_tuple")
fn decode_tuple(
  value: Dynamic,
) -> Result(List(Dynamic), List(dynamic.DecodeError))

@external(erlang, "pretty_diff_classify_ffi", "decode_nil")
@external(javascript, "../../pretty_diff_classify_ffi.mjs", "decode_nil")
fn decode_nil(value: Dynamic) -> Result(Nil, List(dynamic.DecodeError))

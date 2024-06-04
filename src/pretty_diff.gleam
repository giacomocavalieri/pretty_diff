import classify.{
  BitArrayValue, BoolValue, CustomValue, DictValue, FloatValue, ForeignValue,
  IntValue, ListValue, NilValue, StringValue, TupleValue,
}
import glam/doc.{type Document}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// The diff between two Gleam values.
///
@internal
pub type Diff {
  /// When two values have different types, so it doesn't make sense to try and
  /// compute their diff any further.
  /// This can happen if you're trying to get the diff of two dynamic values
  /// with different underlying types.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff(dynamic.from(1), dynamic.from("a"))
  /// // -> DifferentTypes(dynamic.from(1), dynamic.from("a"))
  /// ```
  ///
  DifferentTypes(left: Dynamic, right: Dynamic)

  /// When two values are the same.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff(1, 1)
  /// // -> Equal(dynamic.from(1))
  /// ```
  ///
  Equal(value: Dynamic)

  /// The diff of two different integers.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff(1, 2)
  /// // -> Ints(1, 2)
  /// ```
  ///
  Ints(left: Int, right: Int)

  /// The diff of two different strings.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff("a", "b")
  /// // -> Strings("a", "b")
  /// ```
  ///
  Strings(left: String, right: String)

  /// The diff of two different floats.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff(1.1, 2.2)
  /// // -> Floats(1.1, 2.2)
  /// ```
  ///
  /// > ⚠️ On the JavaScript target, floating point numbers and integers both
  /// > are turned into js `number`s. This means that sometimes when comparing
  /// > two floats that look like integer numbers you might get an `Ints` diff
  /// > instead of a `Floats` one:
  /// >
  /// > ```gleam
  /// > pretty_diff.diff(1.0, 2.0)
  /// > // -> Ints(1, 2)
  /// > ```
  ///
  Floats(left: Float, right: Float)

  /// The diff of two different booleans.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff(True, False)
  /// // -> Bools(True, False)
  /// ```
  ///
  Bools(left: Bool, right: Bool)

  /// The diff of two different bit arrays.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff(<<1:1>>, <<0:1>>)
  /// // -> BitArrays(<<1:1>>, <<0:1>>)
  /// ```
  ///
  /// > ⚠️ On the Erlang target, strings are represented as bit arrays.
  /// > This means that sometimes when comparing two bit arrays that look like
  /// > valid utf-8 strings you might get a `Strings` diff instead of a
  /// > `BitArrays` one:
  /// >
  /// > ```gleam
  /// > pretty_diff.diff(<<65>>, <<66>>)
  /// > // -> Strings("A", "B")
  /// > ```
  ///
  BitArrays(left: BitArray, right: BitArray)

  /// The diff of two different lists. In this case you get a list of `ItemDiff`
  /// that tells you if an item is shared, only in the first list, or only in
  /// the second one.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff([1, 2, 3], [0, 2, 3, 4])
  /// // -> Lists([
  /// //   Shared(1, Ints(1, 0)),
  /// //   Shared(2, Equal(2)),
  /// //   Shared(3, Equal(3)),
  /// //   Right(4, 4),
  /// // ])
  /// ```
  ///
  Lists(List(ItemDiff))

  /// The diff of two different tuples. In this case you get a list of
  /// `ItemDiff` that tells you if an item is shared, only in the first tuple,
  /// or only in the second one.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff(#("a", "b", "c"), #("a", "a", "c"))
  /// // -> Tuples([
  /// //   Shared(1, Equal("a")),
  /// //   Shared(2, Strings("b", "a")),
  /// //   Shared(3, Equal("c")),
  /// // ])
  /// ```
  ///
  Tuples(List(ItemDiff))

  /// The diff of two different dictionaries. In this case you'll have the keys
  /// that are only on the first one, those that are on the second one, and the
  /// shared keys associated with the diff of their respective values.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff(
  ///   dict.from_list([#(1, "a"), #(2, "b")]),
  ///   dict.from_list([#(1, "b")]),
  /// )
  /// // -> Dicts(
  /// //   only_left: dict.from_list([#(2, "b")]),
  /// //   only_right: dict.from_list([]),
  /// //   shared: dict.from_list([#(1, Strings("a", "b"))])
  /// // )
  /// ```
  ///
  Dicts(
    only_left: Dict(Dynamic, Dynamic),
    only_right: Dict(Dynamic, Dynamic),
    shared: Dict(Dynamic, Diff),
  )

  /// The diff of two different foreign values. A foreign value might be any
  /// object coming from external code.
  ///
  Foreigns(left: String, right: String)

  /// The diff between two different custom types.
  /// To get a better sense of what that diff might look like you can have a
  /// look at `CustomDiffType`.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff(Ok(1), Ok(2))
  /// // -> CustomTypes(SameConstructors("Ok", [Ints(1, 2)]))
  ///
  /// pretty_diff.diff(Ok(1), Error(2))
  /// // -> CustomTypes(DifferentConstructors(Ok(1), Error(2)))
  /// ```
  ///
  CustomTypes(CustomTypeDiff)
}

/// An item you get when diffing two lists/tuples. It records the position of
/// the element in the list/tuple and wether it belongs to both, or just one of
/// the two.
///
@internal
pub type ItemDiff {
  Left(position: Int, item: Dynamic)
  Right(position: Int, item: Dynamic)
  Shared(position: Int, item: Diff)
}

/// The possible outcome when diffing two values belonging to a custom type.
///
@internal
pub type CustomTypeDiff {
  /// When the two values were built using different constructors, they are
  /// considered trivially different and it doesn't make sense to try and diff
  /// them any further.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff(Ok(1), Error(2))
  /// // -> CustomTypes(DifferentConstructors(Ok(1), Error(2)))
  /// ```
  ///
  DifferentConstructors(one: Dynamic, other: Dynamic)

  /// When the two values were built using the same constructor. In this case
  /// their fields are also compared and diffed.
  ///
  /// ## Examples
  ///
  /// ```gleam
  /// pretty_diff.diff(Ok(1), Ok(2))
  /// // -> CustomTypes(SameConstructors("Ok", [Ints(1, 2)]))
  /// ```
  ///
  SameConstructors(name: String, fields: List(Diff))
}

/// Returns a pretty printed string representing the diff between two values.
///
/// > ⚠️ This pretty printed string is meant to be displayed for human
/// > consumption. Its look might change without any major version bump, so your
/// > code should not depend on its specific formatting.
///
/// ## Examples
///
/// ```
/// diff([1, 2, 3, 4, 5], [1, 2, 3, 4, 6]) |> to_string
/// // -> [1, ..., 4, -5 +6]
/// ```
///
pub fn from(left: a, and right: a) -> String {
  diff(left, right) |> to_doc |> doc.to_string(80)
}

/// Get the diff of two values.
///
@internal
pub fn diff(between one: a, and other: a) -> Diff {
  let dynamic_one = dynamic.from(one)
  let dynamic_other = dynamic.from(other)

  use <- bool.guard(one == other, return: Equal(dynamic_one))

  // If the two values are not trivially equal then we need to check their
  // actual value and compare them to provide a nice diff.
  // Even though the type signature would make sure that `one` and `other`
  // always have the same type (and so if I have a `IntValue` on the left, I'll
  // have one on the right as well) I still need to check for type mismatches
  // and report them accordingly. This is because one could pass in two dynamics
  // with different underlying types!
  //
  case classify.classify(dynamic_one), classify.classify(dynamic_other) {
    IntValue(one), IntValue(other) -> Ints(one, other)
    // We need to special handle this case to make working on the js target a
    // bit easier: if you `diff(1.0, 2.1)` you'd get a `IntValue` and
    // `FloatValue`, so we have to manually cast the int to a float.
    //
    // This has the tradeoff that if someone does
    // `diff(dynamic.from(1), dynamic.from(2.1))` on the Erlang target they
    // won't get a type mismatch error, but a `Floats(1.0, 2.1)`.
    // I think this is totally acceptable.
    IntValue(one), FloatValue(other) -> Floats(int.to_float(one), other)
    IntValue(_), _ -> DifferentTypes(dynamic_one, dynamic_other)

    StringValue(one), StringValue(other) -> Strings(one, other)
    StringValue(_), _ -> DifferentTypes(dynamic_one, dynamic_other)

    FloatValue(one), FloatValue(other) -> Floats(one, other)
    // We do this for the same exact reason of the
    // `IntValue(_), FloatValue(_)` case.
    FloatValue(one), IntValue(other) -> Floats(one, int.to_float(other))
    FloatValue(_), _ -> DifferentTypes(dynamic_one, dynamic_other)

    BoolValue(one), BoolValue(other) -> Bools(one, other)
    BoolValue(_), _ -> DifferentTypes(dynamic_one, dynamic_other)

    BitArrayValue(one), BitArrayValue(other) -> BitArrays(one, other)
    BitArrayValue(_), _ -> DifferentTypes(dynamic_one, dynamic_other)

    ListValue(one), ListValue(other) -> Lists(histogram(one, other))
    ListValue(_), _ -> DifferentTypes(dynamic_one, dynamic_other)

    TupleValue(one), TupleValue(other) -> diff_tuples(one, other)
    TupleValue(_), _ -> DifferentTypes(dynamic_one, dynamic_other)

    DictValue(one), DictValue(other) -> diff_dicts(one, other)
    DictValue(_), _ -> DifferentTypes(dynamic_one, dynamic_other)

    ForeignValue(one), ForeignValue(other) -> Foreigns(one, other)
    ForeignValue(_), _ -> DifferentTypes(dynamic_one, dynamic_other)

    CustomValue(one_name, one_fields), CustomValue(other_name, other_fields) ->
      diff_custom_types(
        dynamic_one,
        one_name,
        one_fields,
        dynamic_other,
        other_name,
        other_fields,
      )
    CustomValue(_, _), _ -> DifferentTypes(dynamic_one, dynamic_other)

    // This should never happen as we return early if two values are the same.
    NilValue, NilValue -> panic as "nil should always equal nil"
    NilValue, _ -> DifferentTypes(dynamic_one, dynamic_other)
  }
}

fn diff_tuples(one: List(Dynamic), other: List(Dynamic)) -> Diff {
  let diff_items = case list.length(one) == list.length(other) {
    // If the two tuples have different sizes we diff them as if they were
    // lists. Despite being rare (someone would have to pass in dynamics to be
    // able to compare two tuples with different sizes).
    False -> histogram(one, other)

    // It is far more common to compare two tuples with the same size.
    // In this case we want to compare each of their items one by one and show a
    // proper diff for each pair.
    True -> {
      use #(one, other), i <- list.index_map(list.zip(one, other))
      use <- bool.guard(when: one == other, return: Shared(i + 1, Equal(one)))
      Shared(i + 1, diff(one, other))
    }
  }
  Tuples(diff_items)
}

fn diff_dicts(
  one: Dict(Dynamic, Dynamic),
  other: Dict(Dynamic, Dynamic),
) -> Diff {
  let left = subtract(from: one, this: other)
  let right = subtract(from: other, this: one)
  Dicts(only_left: left, only_right: right, shared: intersection(one, other))
}

fn diff_custom_types(
  one: Dynamic,
  one_name: String,
  one_fields: List(classify.Field),
  other: Dynamic,
  other_name: String,
  other_fields: List(classify.Field),
) -> Diff {
  case one_name == other_name {
    False -> CustomTypes(DifferentConstructors(one, other))
    True ->
      CustomTypes(SameConstructors(
        name: one_name,
        fields: list.map2(one_fields, other_fields, fn(one, other) {
          diff(one.value, other.value)
        }),
      ))
  }
}

// --- HISTOGRAM DIFFING ALGORITHM ---------------------------------------------

fn histogram(one: List(Dynamic), other: List(Dynamic)) -> List(ItemDiff) {
  match_diff_lines([], lcs(one, other), 1, one, 1, other)
}

fn match_diff_lines(
  lines: List(ItemDiff),
  lcs: List(Dynamic),
  line_one: Int,
  one: List(Dynamic),
  line_other: Int,
  other: List(Dynamic),
) -> List(ItemDiff) {
  case lcs, one, other {
    // We drained all the lines, we can return the accumulator which was built
    // in reverse order.
    [], [], [] -> list.reverse(lines)

    // If we no longer have common lines in the common prefix we get the diff
    // of all pairs of items from the two lists.
    [], [first_one, ..one], [first_other, ..other] ->
      [Shared(line_one, diff(first_one, first_other)), ..lines]
      |> match_diff_lines(lcs, line_one + 1, one, line_other + 1, other)

    // If we don't have any more lines in the common prefix we first drain all
    // the lines from the first list marking those as old.
    [], [first, ..one], [] ->
      [Left(line_one, first), ..lines]
      |> match_diff_lines(lcs, line_one + 1, one, line_other, other)

    // If we've also drained the first list we finish by draining the other one
    // marking all its lines as new ones.
    [], [], [first, ..other] ->
      [Right(line_other, first), ..lines]
      |> match_diff_lines(lcs, line_one, one, line_other + 1, other)

    // If both lists have something at the top that is not shared that means
    // that's an item that changed between the two lists. So we take a diff of
    // it.
    [first_common, ..], [first_one, ..one], [first_other, ..other]
      if first_common != first_one && first_common != first_other
    ->
      [Shared(line_one, diff(first_one, first_other)), ..lines]
      |> match_diff_lines(lcs, line_one + 1, one, line_other + 1, other)

    // While the first list has lines that are not in common we add those
    // marking them as old.
    [first_common, ..], [first_one, ..one], other if first_common != first_one ->
      [Left(line_one, first_one), ..lines]
      |> match_diff_lines(lcs, line_one + 1, one, line_other, other)

    // While the second list has lines that are not in common we add those
    // marking them as new.
    [first_common, ..], one, [first_other, ..other]
      if first_common != first_other
    ->
      [Right(line_other, first_other), ..lines]
      |> match_diff_lines(lcs, line_one, one, line_other + 1, other)

    // In all other cases the line is shared between the two lists.
    [first_common, ..lcs], [_, ..one], [_, ..other] ->
      [Shared(line_other, Equal(first_common)), ..lines]
      |> match_diff_lines(lcs, line_one + 1, one, line_other + 1, other)

    [_, ..], [], _ | [_, ..], _, [] -> panic as "unreachable"
  }
}

/// Find the least common subsequences of shared items between two lists.
///
/// Reference: https://tiarkrompf.github.io/notes/?/diff-algorithm/
///
fn lcs(one: List(a), other: List(a)) -> List(a) {
  // The recursive definition is so intuitive and elegant, just please don't
  // look at how `lowest_occurrence_common_item` is defined.
  //
  // 1. We remove the common prefix and suffix from the lists.
  // 2. In the remaining lists we find the common element that appears in both
  //    the list number of times.
  // 3. We recursively look for the `lcs` of the pieces that come before the
  //    common element in both lists and the pieces that come after the common
  //    element in both lists.
  // 4. We join the common prefix and suffix, `lcs`s and common item.

  let #(prefix, one, other) = pop_common_prefix(between: one, and: other)
  let #(suffix, one, other) = pop_common_suffix(between: one, and: other)

  // 💡 A possible optimisation could be using a cache and hit that before
  // calling this function. That might make things faster as well.
  case lowest_occurrence_common_item(one, other) {
    None -> list.concat([prefix, suffix])
    Some(#(item, _, before_a, after_a, before_b, after_b)) ->
      // 💡 A possible optimisation I want to look into is using bags (super
      // fast append only) and turn that into a list only after everything is
      // done. That way we could avoid always repeatedly appending lists.
      list.concat([
        prefix,
        lcs(list.reverse(before_a), list.reverse(before_b)),
        [item],
        lcs(after_a, after_b),
        suffix,
      ])
  }
}

type Occurs(a) {
  One(times: Int, before: List(a), after: List(a))
  Other(times: Int, before: List(a), after: List(a))
  Both(
    times: Int,
    before_one: List(a),
    after_one: List(a),
    before_other: List(a),
    after_other: List(a),
  )
}

fn lowest_occurrence_common_item(
  between one: List(a),
  and other: List(a),
) -> Option(#(a, Int, List(a), List(a), List(a), List(a))) {
  let histogram =
    histogram_add(to: dict.new(), from: one, with: One, acc: [])
    |> histogram_add(from: other, with: Other, acc: [])

  use lowest, a, occurs <- dict.fold(over: histogram, from: None)
  case occurs {
    // We're only looking for items that appear in both.
    One(..) | Other(..) -> lowest
    Both(n, before_one, after_one, before_other, after_other) ->
      case lowest {
        None -> Some(#(a, n, before_one, after_one, before_other, after_other))
        // We keep the one that appears the least, so we compare `n` and `m`,
        // that is the number of occurrences of the current lowest and the new
        // item.
        Some(#(_, m, _, _, _, _)) ->
          case m <= n {
            True -> lowest
            False ->
              #(a, n, before_one, after_one, before_other, after_other)
              |> Some
          }
      }
  }
}

fn histogram_add(
  to histogram: Dict(a, Occurs(a)),
  from list: List(a),
  with to_occurrence: fn(Int, List(a), List(a)) -> Occurs(a),
  acc reverse_prefix: List(a),
) -> Dict(a, Occurs(a)) {
  case list {
    [] -> histogram
    [first, ..rest] ->
      {
        use previous <- dict.update(in: histogram, update: first)
        let new_occurrence = to_occurrence(1, reverse_prefix, rest)
        case previous {
          Some(occurrence) -> sum_occurrences(occurrence, new_occurrence)
          None -> new_occurrence
        }
      }
      |> histogram_add(rest, to_occurrence, [first, ..reverse_prefix])
  }
}

// This is not general purpose and only takes into accounts the particular cases
// that might occur in the histogram building. In particular we first add all
// the `One`s and then all the `Other`s.
fn sum_occurrences(one: Occurs(a), other: Occurs(a)) -> Occurs(a) {
  case one, other {
    One(n, _, _), One(m, before, after) -> One(n + m, before, after)
    Other(n, _, _), Other(m, before, after) -> Other(n + m, before, after)

    One(n, before_one, after_one), Other(m, before_other, after_other)
    | Both(n, before_one, after_one, _, _), Other(m, before_other, after_other)
    -> Both(n + m, before_one, after_one, before_other, after_other)

    _, _ -> panic as "unreachable: sum_occurrences"
  }
}

/// Returns the common prefix between two lists, and the remaining lists after
/// removing the common prefix from each one.
///
fn pop_common_prefix(
  between one: List(a),
  and other: List(a),
) -> #(List(a), List(a), List(a)) {
  let #(reverse_prefix, one, other) = do_pop_common_prefix([], one, other)
  #(list.reverse(reverse_prefix), one, other)
}

fn do_pop_common_prefix(
  reverse_prefix: List(a),
  one: List(a),
  other: List(a),
) -> #(List(a), List(a), List(a)) {
  case one, other {
    [first_one, ..one], [first_other, ..other] if first_one == first_other ->
      do_pop_common_prefix([first_one, ..reverse_prefix], one, other)
    _, _ -> #(reverse_prefix, one, other)
  }
}

/// Returns the common suffix between two lists, and the remaining lists after
/// removing the common suffix from each one.
///
fn pop_common_suffix(
  between one: List(a),
  and other: List(a),
) -> #(List(a), List(a), List(a)) {
  let #(suffix, reverse_one, reverse_other) =
    do_pop_common_prefix([], list.reverse(one), list.reverse(other))
  #(suffix, list.reverse(reverse_one), list.reverse(reverse_other))
}

// --- DICT UTIL FUNCTIONS -----------------------------------------------------

fn subtract(from one: Dict(a, b), this other: Dict(a, b)) -> Dict(a, b) {
  use acc, key, _value <- dict.fold(over: other, from: one)
  dict.delete(acc, key)
}

fn intersection(between one: Dict(a, b), and other: Dict(a, b)) -> Dict(a, Diff) {
  use acc, key, one_value <- dict.fold(over: one, from: dict.new())
  case dict.get(other, key) {
    Ok(other_value) -> dict.insert(acc, key, diff(one_value, other_value))
    Error(_) -> acc
  }
}

// --- PRETTY PRINTING THE DIFF ------------------------------------------------

fn to_doc(diff: Diff) -> Document {
  case diff {
    Equal(value) -> doc.from_string(string.inspect(value))

    Foreigns(left, right) -> side_by_side(left, right)

    DifferentTypes(left, right) ->
      side_by_side(string.inspect(left), string.inspect(right))

    Ints(left, right) -> side_by_side(int.to_string(left), int.to_string(right))

    Strings(left, right) ->
      side_by_side("\"" <> left <> "\"", "\"" <> right <> "\"")

    Floats(left, right) ->
      side_by_side(float.to_string(left), float.to_string(right))

    Bools(left, right) ->
      side_by_side(bool.to_string(left), bool.to_string(right))

    BitArrays(left, right) ->
      side_by_side(string.inspect(left), string.inspect(right))

    Lists(diff_items) -> diff_lists_doc(diff_items)

    Tuples(diff_items) -> diff_tuples_doc(diff_items)

    Dicts(only_left: only_left, only_right: only_right, shared: shared) ->
      diff_dics_doc(only_left, only_right, shared)

    CustomTypes(SameConstructors(name, fields)) ->
      diff_same_constructors_doc(name, fields)

    CustomTypes(DifferentConstructors(left, right)) ->
      side_by_side(string.inspect(left), string.inspect(right))
  }
}

fn diff_same_constructors_doc(name: String, fields: List(Diff)) -> Document {
  let open_doc = doc.from_string(name <> "(")
  let close_doc = doc.from_string(")")
  comma_separated_docs(open_doc, list.map(fields, to_doc), close_doc)
}

fn diff_dics_doc(
  only_left: Dict(Dynamic, Dynamic),
  only_right: Dict(Dynamic, Dynamic),
  shared: Dict(Dynamic, Diff),
) -> Document {
  let dynamic_to_doc = fn(value) { string.inspect(value) |> doc.from_string }
  let pairs_docs =
    shrink_shared_dict_keys(shared)
    |> fold_pairs(only_left, dynamic_to_doc, left_doc)
    |> fold_pairs(only_right, dynamic_to_doc, right_doc)

  let open_doc = doc.from_string("dict.from_list([")
  let close_doc = doc.from_string("])")
  comma_separated_docs(open_doc, pairs_docs, close_doc)
}

fn fold_pairs(
  from acc: List(Document),
  over dict: Dict(Dynamic, value),
  with value_to_doc: fn(value) -> Document,
  wrap wrap_doc: fn(Document) -> Document,
) -> List(Document) {
  use docs, key, value <- dict.fold(over: dict, from: acc)
  let key_doc = doc.from_string(string.inspect(key))
  let value_doc = value_to_doc(value)
  [to_tuple_doc(key_doc, value_doc) |> wrap_doc, ..docs]
}

fn shrink_shared_dict_keys(shared: Dict(Dynamic, Diff)) -> List(Document) {
  let dynamic_to_doc = fn(value) { string.inspect(value) |> doc.from_string }
  let #(shared, docs) = {
    use #(shared, docs), key, value <- dict.fold(over: shared, from: #([], []))
    let tuple_doc = to_tuple_doc(dynamic_to_doc(key), to_doc(value))
    case value {
      Equal(_) -> #([tuple_doc, ..shared], docs)
      _ -> #(shared, [tuple_doc, ..docs])
    }
  }

  case shared {
    [first, second, _, ..] -> [first, ellipsis(), second, ..docs]
    _ -> shared |> list.append(docs)
  }
}

fn to_tuple_doc(key: Document, value: Document) -> Document {
  let open_doc = doc.from_string("#(")
  let close_doc = doc.from_string(")")
  comma_separated_docs(open_doc, [key, value], close_doc)
}

fn diff_lists_doc(items: List(ItemDiff)) -> Document {
  let open_doc = doc.from_string("[")
  let close_doc = doc.from_string("]")
  comma_separated_docs(open_doc, shrink_shared_diff_items(items), close_doc)
}

fn diff_tuples_doc(items: List(ItemDiff)) -> Document {
  let open_doc = doc.from_string("#(")
  let close_doc = doc.from_string(")")
  comma_separated_docs(open_doc, shrink_shared_diff_items(items), close_doc)
}

fn comma_separated_docs(
  open: Document,
  docs: List(Document),
  close: Document,
) -> Document {
  [
    open,
    [doc.soft_break, doc.join(docs, with: doc.break(", ", ","))]
      |> doc.concat
      |> doc.nest(by: 2),
    doc.break("", ","),
    close,
  ]
  |> doc.concat
  |> doc.group
}

type Shrinked(a, b) {
  NotShrinked(List(a))
  Shrinked(b, b)
}

fn is_shared(item: ItemDiff) -> Bool {
  case item {
    Left(..) | Right(..) -> True
    Shared(..) -> False
  }
}

fn shrink_shared_diff_items(items: List(ItemDiff)) -> List(Document) {
  use chunk <- list.flat_map(list.chunk(items, is_shared))

  case shrink_chunk(chunk) {
    Shrinked(first, last) -> [
      doc.from_string(string.inspect(first)),
      ellipsis(),
      doc.from_string(string.inspect(last)),
    ]

    NotShrinked(items) ->
      list.map(items, fn(item) {
        case item {
          Shared(item: item, ..) -> to_doc(item)
          Left(item: item, ..) ->
            left_doc(doc.from_string(string.inspect(item)))
          Right(item: item, ..) ->
            right_doc(doc.from_string(string.inspect(item)))
        }
      })
  }
}

fn shrink_chunk(items: List(ItemDiff)) -> Shrinked(ItemDiff, Diff) {
  case items {
    [Shared(item: first_item, ..), _, _, Shared(item: fourth_item, ..), ..rest] ->
      case list.last(rest) {
        Ok(Shared(item: last_item, ..)) -> Shrinked(first_item, last_item)
        _ -> Shrinked(first_item, fourth_item)
      }
    _ -> NotShrinked(items)
  }
}

fn left_doc(doc: Document) -> Document {
  [
    doc.zero_width_string("\u{001b}[31m"),
    doc |> doc.prepend(doc.from_string("-")),
    doc.zero_width_string("\u{001b}[39m"),
  ]
  |> doc.concat
}

fn right_doc(doc: Document) -> Document {
  [
    doc.zero_width_string("\u{001b}[32m"),
    doc |> doc.prepend(doc.from_string("+")),
    doc.zero_width_string("\u{001b}[39m"),
  ]
  |> doc.concat
}

fn ellipsis() -> Document {
  [
    doc.zero_width_string("\u{001b}[2m"),
    doc.from_string("..."),
    doc.zero_width_string("\u{001b}[22m"),
  ]
  |> doc.concat
}

fn side_by_side(left: String, right: String) -> Document {
  [
    left_doc(doc.from_string(left)),
    doc.space,
    right_doc(doc.from_string(right)),
  ]
  |> doc.concat
  |> doc.group
}

import * as $stdlib from "../gleam_stdlib/gleam_stdlib.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import * as $gleam from "./gleam.mjs";
import * as $classify from "./pretty_diff/internal/classify.mjs";

export function decode_custom_type(value) {
  if (value instanceof $gleam.CustomType) {
    const name = value.constructor.name;
    const fields = Object.keys(value).map((label) => {
      const field_label = isNaN(parseInt(label)) ? new $option.Some(label) : new $option.None();
      return new $classify.Field(field_label, value[label]);
    });
    return new $gleam.Ok(new $classify.CustomValue(name, $gleam.toList(fields)));
  } else {
    return new $gleam.Error();
  }
}

export function decode_tuple(value) {
  return Array.isArray(value) ? new $gleam.Ok($gleam.toList(value)) : new $gleam.Error();
}

export function decode_nil(value) {
  return value === undefined ? new $gleam.Ok() : new $gleam.Error();
}

import * as $stdlib from "../gleam_stdlib/gleam_stdlib.mjs";
import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import * as $gleam from "./gleam.mjs";
import * as $classify from "./pretty_diff/internal/classify.mjs";

function decoder_error(expected, got) {
  return decoder_error_no_classify(expected, $stdlib.classify_dynamic(got));
}

function decoder_error_no_classify(expected, got) {
  return new $gleam.Error($gleam.List.fromArray([new $dynamic.DecodeError(expected, got, $gleam.toList([]))]));
}

export function decode_custom_type(value) {
  if (value instanceof $gleam.CustomType) {
    const name = value.constructor.name;
    const fields = Object.keys(value).map((label) => {
      return new $classify.Field(isNaN(parseInt(label)) ? new $option.Some(label) : new $option.None(), value[label]);
    });

    return new $gleam.Ok(new $classify.CustomValue(name, $gleam.toList(fields)));
  }

  return decoder_error("CustomType", value);
}

export function decode_tuple(value) {
  if (Array.isArray(value)) return new $gleam.Ok($gleam.toList(value));
  return decoder_error("Tuple", value);
}

export function decode_nil(value) {
  if (value === undefined) return new $gleam.Ok();
  return decoder_error("Nil", value);
}

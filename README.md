# pretty_diff

[![Package Version](https://img.shields.io/hexpm/v/pretty_diff)](https://hex.pm/packages/pretty_diff)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/pretty_diff/)
![Supported targets](https://img.shields.io/badge/supports-all_targets-ffaff3)

⚖️ Pretty printing the difference between Gleam values

## Installation

```sh
gleam add pretty_diff
```

## Usage

```gleam
import pretty_diff

pub fn main() {
  pretty_diff.from(Ok("old"), and: Ok("new"))
  // -> Ok(-"old" +"new")
}
```

# bitbuilder
Simple C++20 library to specify and build bitfields at compile time. Works for `clang` and `gcc`

## Example
```cpp
build_pattern<"#05jjj00000001">(bitarg<"jjj#*5", uint8_t>(0b0000'0111));
```

## Syntax
- A `simple-field` is either `a-zA-Z*` (for input specification), or `a-zA-Z01` (for output specification)
- A `expand-field` is `# simple-field N`, where `N` is a number matching `\d+`. This is equivalent to repeating the `simple-field` by `N` times.
- A `bitfield-specifier` is a sequence of `simple-field` or `expand-field`, such that the total expanded length is equivalent to the bit-length of the output (for `build_pattern<...>`), or the bit-length of the unsigned int passed into `bitarg<...>`

# bitbuilder
Simple C++20 library to specify and build bitfields at compile time. Works for `clang` and `gcc`

## Features
- decode input `uint` as bitfield
- encode bitfield from various inputs
- reversing bit-order of input field
- very efficient code generation

## Example
```cpp
build_pattern<"#05jjj00000001">(bitarg<"jjj#*5", uint8_t>(0b0000'0111));
```

## Syntax
- A `simple-field` is either `a-zA-Z*` (for input specification), or `a-zA-Z01` (for output specification)
- A `expand-field` is `# simple-field N`, where `N` is a number matching `\d+`. This is equivalent to repeating the `simple-field` by `N` times.
For example, `#05jjj00000001` is the same as `00000jjj00000001`
- A `bitfield-specifier` is a sequence of `simple-field` or `expand-field`, such that the total expanded length is equivalent to the bit-length of 
the output (for `build_pattern<...>`), or the bit-length of the unsigned int passed into `bitarg<...>`

## Performance
This library generates very fast code with negligible overhead compared to hand-crafted bit manipulation:

The following:
```cpp
#include "bitbuilder.h"
#include <iostream>

uint16_t do_encode(uint8_t val)
{
    using namespace bitbuilder;
    return build_pattern<"#05JJJ00000001">(bitarg<"JJJ#*5", uint8_t>(val));
}
```
Will generate:
```
$ clang++ test.cpp -c -o do_encode.o -std=c++20 -O3
$ objdump -D do_encode.o
0000000000000000 <_Z9do_encodeh>:
   0:	8d 04 fd 00 00 00 00 	lea    0x0(,%rdi,8),%eax
   7:	24 20                	and    $0x20,%al
   9:	89 f9                	mov    %edi,%ecx
   b:	c0 e1 05             	shl    $0x5,%cl
   e:	80 e1 40             	and    $0x40,%cl
  11:	08 c1                	or     %al,%cl
  13:	0f b6 c1             	movzbl %cl,%eax
  16:	83 e7 01             	and    $0x1,%edi
  19:	c1 e7 07             	shl    $0x7,%edi
  1c:	01 f8                	add    %edi,%eax
  1e:	05 00 80 00 00       	add    $0x8000,%eax
  23:	c3                   	ret
```
And
```
$ g++ test.cpp -c -o do_encode.o -std=c++20 -O3
$ objdump -D do_encode.o
0000000000000000 <_Z9do_encodeh>:
   0:	89 f8                	mov    %edi,%eax
   2:	89 fa                	mov    %edi,%edx
   4:	40 d0 ef             	shr    %dil
   7:	c0 e8 02             	shr    $0x2,%al
   a:	83 e2 01             	and    $0x1,%edx
   d:	83 e7 01             	and    $0x1,%edi
  10:	c1 e2 07             	shl    $0x7,%edx
  13:	83 e0 01             	and    $0x1,%eax
  16:	c1 e7 06             	shl    $0x6,%edi
  19:	c1 e0 05             	shl    $0x5,%eax
  1c:	09 d0                	or     %edx,%eax
  1e:	09 f8                	or     %edi,%eax
  20:	66 0d 00 80          	or     $0x8000,%ax
  24:	c3                   	ret
```


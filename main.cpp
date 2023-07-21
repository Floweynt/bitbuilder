#include "bitbuilder.h"
#include <iostream>

int main()
{
    using namespace bitbuilder;
    std::cout << std::hex << build_pattern<"#05jjj00000001">(bitarg<"jjj#*5", uint8_t>(0b0000'0111));
}

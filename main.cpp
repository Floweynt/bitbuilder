#include "bitbuilder.h"
#include <iostream>

int main()
{
    using namespace bitbuilder;
    std::cout << std::hex << build_pattern<"#05JJJ00000001">(bitarg<"JJJ#*5", uint8_t>(0b0000'0110));
}

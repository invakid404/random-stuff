#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
using num = long long;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    num a, b;

    std::cin >> a >> b;
    if (a > b) {
        std::swap(a, b);
    }

    auto g = std::__gcd(a, b);

    auto p = a / g;
    auto q = b / g;

    auto y = (q - 1) / p;
    y = std::min(y, a / q);

    std::cout << y << ENDL;

    return 0;
}
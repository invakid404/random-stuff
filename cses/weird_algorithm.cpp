#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
using num = unsigned long long;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    num n;
    std::cin >> n;

    std::cout << n << " ";

    while (n != 1) {
        if (n & 1) {
            n *= 3;
            ++n;
        } else {
            n /= 2;
        }

        std::cout << n << " ";
    }

    std::cout << ENDL;

    return 0;
}
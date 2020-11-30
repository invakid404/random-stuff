#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
using num = long long;

auto solve(num x, num y) {
    auto flag = true;

    if (x > y) {
        std::swap(x, y);
        flag = false;
    }

    auto odd = y & 1;

    if (odd == flag) {
        return y * y - x + 1;
    }

    return --y * y + x;
}
}  // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int t;
    std::cin >> t;

    while (t--) {
        num x, y;
        std::cin >> x >> y;

        std::cout << solve(x, y) << ENDL;
    }

    return 0;
}
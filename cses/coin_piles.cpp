#include <bits/stdc++.h>

#define ENDL '\n'

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int t;
    std::cin >> t;

    while (t--) {
        int a, b;
        std::cin >> a >> b;

        auto res = true;

        auto x = 2 * a - b;
        auto y = 2 * b - a;

        res = x >= 0 && x % 3 == 0 && y >= 0 && y % 3 == 0;
        std::cout << (res ? "YES" : "NO") << ENDL;
    }

    return 0;
}
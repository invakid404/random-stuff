#include <bits/stdc++.h>

#define ENDL '\n'

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    std::cin >> n;

    for (auto i = 1LL; i <= n; ++i) {
        auto s = i * i;
        std::cout << (s * (s - 1)) / 2 - 4 * (i - 1) * (i - 2) << ENDL;
    }

    return 0;
}
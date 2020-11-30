#include <bits/stdc++.h>

#define ENDL '\n'

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    std::cin >> n;

    auto res = 1;
    for (auto i = 2; i <= n; ++i) {
        int foo;
        std::cin >> foo;

        res += i;
        res -= foo;
    }

    std::cout << res << ENDL;

    return 0;
}
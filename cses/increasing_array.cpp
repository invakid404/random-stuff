#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
using num = unsigned long long;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    std::cin >> n;

    std::vector<num> v;
    v.reserve(n);

    for (auto i = 0; i < n; ++i) {
        num foo;
        std::cin >> foo;

        v.emplace_back(foo);
    }

    num res = 0;
    for (auto i = 0; i < n - 1; ++i) {
        if (v[i] > v[i + 1]) {
            res += v[i] - v[i + 1];
            v[i + 1] = v[i];
        }
    }

    std::cout << res << ENDL;

    return 0;
}
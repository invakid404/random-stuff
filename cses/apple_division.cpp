#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
using num = long long;

auto solve(std::vector<num>& vec, int idx, num ls, num rs) -> num {
    return idx >= vec.size() ? std::abs(ls - rs)
                             : std::min(solve(vec, idx + 1, ls + vec[idx], rs),
                                        solve(vec, idx + 1, ls, rs + vec[idx]));
}
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    std::cin >> n;

    std::vector<num> vec;
    vec.reserve(n);

    num foo;
    for (auto i = 0; i < n; ++i) {
        std::cin >> foo;
        vec.emplace_back(foo);
    }

    std::cout << solve(vec, 0, 0, 0) << ENDL;

    return 0;
}
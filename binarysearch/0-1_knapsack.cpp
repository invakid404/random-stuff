#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
auto solve(std::vector<int>& weights, std::vector<int>& values, int capacity) {
    std::vector<int> dp(capacity + 1);

    auto n = static_cast<int>(weights.size());
    for (auto i = 0; i < n; ++i) {
        for (auto j = capacity; j >= weights[i]; --j) {
            dp[j] = std::max(dp[j], dp[j - weights[i]] + values[i]);
        }
    }

    return dp[capacity];
}
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    std::cin >> n;

    std::vector<int> weights, values;
    weights.reserve(n);
    values.reserve(n);

    int foo;
    for (auto i = 0; i < n; ++i) {
        std::cin >> foo;
        weights.emplace_back(foo);
    }

    for (auto i = 0; i < n; ++i) {
        std::cin >> foo;
        values.emplace_back(foo);
    }

    int capacity;
    std::cin >> capacity;

    std::cout << solve(weights, values, capacity) << ENDL;

    return 0;
}

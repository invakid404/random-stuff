// A3 http://www.math.bas.bg/infos/EK_2018_NOI_1/u/A.pdf
#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
auto mex(std::unordered_set<int> const& nums) {
    for (auto i = 0;; ++i) {
        if (!nums.count(i)) {
            return i;
        }
    }
}

auto precompute(int a, int b) {
    std::vector<int> dp(a + b, -1);

    std::function<int(int)> solve;
    solve = [&](int n) {
        if (n < 0) {
            return -1;
        }

        auto& res = dp[n];
        if (res == -1) {
            std::unordered_set<int> states = {solve(n - a), solve(n - b)};

            res = mex(states);
        }

        return res;
    };

    for (auto i = 0; i < dp.size(); ++i) {
        solve(i);
    }

    return dp;
}

auto mod(std::string const& s, int k) {
    auto res = 0;
    for (auto& c : s) {
        res = (res * 10 + c - '0') % k;
    }

    return res;
}
}  // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int a, b;
    std::cin >> a >> b;

    auto dp = precompute(a, b);

    int m;
    std::cin >> m;

    while (m--) {
        std::string curr;
        std::cin >> curr;

        auto v = mod(curr, a + b);

        std::cout << (dp[v] > 0 ? 1 : 2) << ENDL;
    }

    return 0;
}
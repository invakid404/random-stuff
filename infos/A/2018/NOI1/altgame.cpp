#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
auto mex(std::unordered_set<int>& x) {
    auto i = 0;
    while (x.find(i) != x.end()) {
        ++i;
    }

    return i;
}

auto precompute(int a, int b) {
    std::vector<int> dp(a + b, -1);
    std::fill(dp.begin(), dp.begin() + a, 0);

    std::function<int(int)> sg;
    sg = [&](int n) {
        auto& res = dp[n];
        if (res == -1) {
            std::unordered_set<int> children;
            if (n - a >= 0) {
                children.emplace(sg(n - a));
            }
            if (n - b >= 0) {
                children.emplace(sg(n - b));
            }

            res = mex(children);
        }

        return res;
    };

    for (auto i = 0; i < dp.size(); ++i) {
        sg(i);
    }

    return dp;
}

auto mod(std::string& s, int m) {
    auto res = 0;
    for (auto& val : s) {
        res = (res * 10 + val - '0') % m;
    }

    return res;
}
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int a, b;
    std::cin >> a >> b;

    if (a > b) {
        std::swap(a, b);
    }

    auto sol = precompute(a, b);

    int m;
    std::cin >> m;

    std::string curr;
    while (m--) {
        std::cin >> curr;
        auto v = mod(curr, a + b);

        std::cout << (sol[v] > 0 ? 1 : 2) << ENDL;
    }

    return 0;
}
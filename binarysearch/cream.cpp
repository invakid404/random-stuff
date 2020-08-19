#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class Solution {
public:
    template<class T>
    using vec = std::vector<T>;

    template<class T>
    using mat = vec<vec<T>>;

    auto solve(std::vector<int>& prices, int k) {
        auto n = static_cast<int>(prices.size());
        if (k == 0 || n < 2) {
            return 0;    
        }

        if (k > n / 2) {
            auto res = 0;
            for (auto i = 0; i < n - 1; ++i) {
                if (prices[i + 1] > prices[i]) {
                    res += prices[i + 1] - prices[i];
                }
            }

            return res;
        }

        mat<int> max_with(k + 1, vec<int>(n));
        mat<int> max_without(k + 1, vec<int>(n));

        for (auto i = 1; i <= k; ++i) {
            max_with[i][0] = -prices[0];
            max_without[i][0] = 0;

            for (auto days = 1; days < n; ++days) {
                max_with[i][days] = 
                    std::max(max_with[i][days - 1],
                             max_without[i - 1][days] - prices[days]);

                max_without[i][days] = 
                    std::max(max_without[i][days - 1],
                             max_with[i][days] + prices[days]);
            }
        }

        return max_without[k][n - 1];
    }
};
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    std::cin >> n;

    std::vector<int> prices;
    prices.reserve(n);

    int foo;
    while (n--) {
        std::cin >> foo;
        prices.emplace_back(foo);
    }

    int k;
    std::cin >> k;

    Solution solution;
    std::cout << solution.solve(prices, k) << ENDL;

    return 0;
}
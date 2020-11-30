#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class Solution {
public:
    template <class T>
    using vec = std::vector<T>;

    template <class T>
    using mat = vec<vec<T>>;

    auto solve(std::string& a, std::string& b) {
        auto m = a.size(), n = b.size();
        mat<int> dp(2, vec<int>(m + 1, 0));

        for (auto i = 0; i <= m; ++i) {
            dp[0][i] = i;
        }

        for (auto i = 1; i <= n; ++i) {
            for (auto j = 0; j <= m; ++j) {
                auto curr_i = i % 2;
                auto prev_i = (i - 1) % 2;

                if (j == 0) {
                    dp[curr_i][j] = i;
                } else if (a[j - 1] == b[i - 1]) {
                    dp[curr_i][j] = dp[prev_i][j - 1];
                } else {
                    dp[curr_i][j] =
                        1 + std::min({dp[prev_i][j], dp[curr_i][j - 1],
                                      dp[prev_i][j - 1]});
                }
            }
        }

        return dp[n % 2][m];
    }
};
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    std::string a, b;
    std::cin >> a >> b;

    Solution solution;
    std::cout << solution.solve(a, b) << ENDL;

    return 0;
}
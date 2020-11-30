#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
template <class T>
using vec = std::vector<T>;

template <class T>
using mat = vec<vec<T>>;

class Solution {
public:
    constexpr static int mod = 1000000007;

    int solve(mat<int>& matrix) {
        auto n = static_cast<int>(matrix.size());
        auto m = static_cast<int>(matrix[0].size());

        if (matrix[0][0] || matrix[n - 1][m - 1]) {
            return 0;
        }

        mat<int> dp(n, vec<int>(m, -1));

        std::function<int(int, int)> recurse;
        recurse = [&](int i, int j) {
            if (i == 0 && j == 0) {
                return 1;
            }

            if (i < 0 || i >= n || j < 0 || j >= m || matrix[i][j]) {
                return 0;
            }

            auto res = &dp[i][j];
            if (*res == -1) {
                *res = 0;

                *res = (*res + recurse(i - 1, j)) % mod;
                *res = (*res + recurse(i, j - 1)) % mod;
            }

            return *res;
        };

        return recurse(n - 1, m - 1);
    }
};
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n, m;
    std::cin >> n >> m;

    mat<int> matrix(n, vec<int>(m));
    for (auto i = 0; i < n; ++i) {
        for (auto j = 0; j < m; ++j) {
            std::cin >> matrix[i][j];
        }
    }

    Solution solution;
    std::cout << solution.solve(matrix) << ENDL;

    return 0;
}
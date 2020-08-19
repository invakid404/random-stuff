#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
template<class T>
using vec = std::vector<T>;

template<class T>
using mat = vec<vec<T>>;

// TODO: Bidirectional BFS?
class Solution {
public:
    struct State {
        int i, j, steps;
        State(int i, int j, int steps)
            : i(i), j(j), steps(steps) {}
    };

    const vec<std::pair<int, int>> deltas = 
        {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};

    auto solve(mat<int>& matrix) {
        auto n = static_cast<int>(matrix.size());
        if (n <= 0) {
            return -1;
        }

        auto m = static_cast<int>(matrix[0].size());

        if (matrix[0][0] || matrix[n - 1][m - 1]) {
            return -1;
        }

        std::queue<State> bfs;
        bfs.emplace(0, 0, 1);

        mat<bool> visited(n, vec<bool>(m, false));

        while (!bfs.empty()) {
            auto curr = bfs.front();
            bfs.pop();

            if (visited[curr.i][curr.j]) {
                continue;
            }
            visited[curr.i][curr.j] = true;

            if (matrix[curr.i][curr.j]) {
                continue;
            }

            if (curr.i == n - 1 && curr.j == m - 1) {
                return curr.steps;
            }

            for (auto& delta : this->deltas) {
                auto next_i = curr.i + delta.first;
                auto next_j = curr.j + delta.second;

                if (next_i < 0 || next_i >= n ||
                    next_j < 0 || next_j >= m) {
                    continue;
                }

                bfs.emplace(next_i, next_j, curr.steps + 1);
            }
        }

        return -1;
    }
};
}

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
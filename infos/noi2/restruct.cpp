#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
using num = long long;

template <class T>
using vec = std::vector<T>;

template <class T>
using mat = vec<vec<T>>;

using graph_t = mat<num>;

auto solve_for(graph_t const& tree, num wage, num start_node) {
    vec<bool> visited(tree.size());
    vec<num> wages(tree.size());

    std::queue<num> q;
    q.emplace(start_node);

    visited[start_node] = true;
    wages[start_node] = wage;

    while (!q.empty()) {
        auto u = q.front();
        q.pop();

        for (auto& v : tree[u]) {
            if (visited[v]) {
                continue;
            }

            visited[v] = true;
            wages[v] = wages[u] + wage;

            q.emplace(v);
        }
    }

    num res = 0;
    for (auto u = 0; u < tree.size(); ++u) {
        if (!visited[u]) {
            return std::make_pair(false, res);
        }

        res += wages[u];
    }

    return std::make_pair(true, res);
}

auto solve(graph_t const& tree, num wage) {
    auto res = std::numeric_limits<num>::max();
    for (num u = 0; u < tree.size(); ++u) {
        auto [is_valid, curr_res] = solve_for(tree, wage, u);
        if (!is_valid) {
            continue;
        }

        res = std::min(res, curr_res);
    }

    return res;
}
}  // namespace

int main() {
    std::ios_base::sync_with_stdio(false);

    num n, k;
    std::cin >> n >> k;

    graph_t tree(n);

    num i, v;
    for (num u = 0; u < n; ++u) {
        std::cin >> i;

        while (i--) {
            std::cin >> v;

            tree[v - 1].emplace_back(u);
        }
    }

    std::cout << solve(tree, k) << ENDL;

    return 0;
}

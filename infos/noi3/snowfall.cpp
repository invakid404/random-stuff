#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
template <class T>
using vec = std::vector<T>;

template <class T>
using mat = vec<vec<T>>;

struct edge {
    int to, capacity, id;

    edge(int to, int capacity, int id) : to(to), capacity(capacity), id(id) {}
};

using graph_t = mat<edge>;

auto find_articulation_points(graph_t const& graph, int s) {
    std::vector<int> in(graph.size());
    std::vector<int> up(graph.size());

    auto timer = 0;

    std::vector<bool> visited(graph.size());

    std::vector<int> articulation_points;

    std::function<void(int, int)> dfs;
    dfs = [&](int u, int prev_edge) {
        visited[u] = true;

        in[u] = up[u] = timer++;

        auto flag = 0;
        for (auto& edge : graph[u]) {
            if (edge.capacity <= s) {
                continue;
            }

            if (!visited[edge.to]) {
                dfs(edge.to, edge.id);

                up[u] = std::min(up[u], up[edge.to]);

                if (up[edge.to] >= in[u]) {
                    ++flag;
                }
            } else if (prev_edge != edge.id) {
                up[u] = std::min(up[u], in[edge.to]);
            }
        }

        if (flag > (prev_edge == -1)) {
            articulation_points.emplace_back(u);
        }
    };

    dfs(0, -1);

    auto is_connected = true;
    for (auto val : visited) {
        if (!val) {
            is_connected = false;

            break;
        }
    }

    return std::make_pair(is_connected, articulation_points);
}

auto solve(graph_t const& graph, int k) {
    auto l = 0, r = std::numeric_limits<int>::max();

    while (l <= r) {
        auto m = l + (r - l) / 2;
        auto [is_connected, articulation_points] =
            find_articulation_points(graph, m);
        auto curr_amt = articulation_points.size();

        if (!is_connected || curr_amt >= k) {
            r = m - 1;
        } else {
            l = m + 1;
        }
    }

    auto [is_connected, res_points] = find_articulation_points(graph, l);

    std::sort(res_points.begin(), res_points.end());

    return std::make_pair(is_connected ? l : -1, res_points);
}
}  // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n, m, k;
    std::cin >> n >> m >> k;

    graph_t graph(n);

    int a, b, s;
    for (auto i = 0; i < m; ++i) {
        std::cin >> a >> b >> s;
        --a, --b;

        graph[a].emplace_back(b, s, i);
        graph[b].emplace_back(a, s, i);
    }

    auto [res, articulation_points] = solve(graph, k);
    std::cout << res << " " << articulation_points.size() << ENDL;

    for (auto& val : articulation_points) {
        std::cout << val + 1 << " ";
    }
    std::cout << ENDL;

    return 0;
}

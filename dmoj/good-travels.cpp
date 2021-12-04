#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
using num = long long;

template <class T>
using vec = std::vector<T>;

template <class T>
using mat = vec<vec<T>>;

using graph_t = mat<num>;

auto out_order(graph_t const& graph) {
    vec<num> out;
    out.reserve(graph.size());

    vec<bool> visited(graph.size());

    std::function<void(num)> dfs;
    dfs = [&](num u) {
        visited[u] = true;

        for (auto& v : graph[u]) {
            if (!visited[v]) {
                dfs(v);
            }
        }

        out.emplace_back(u);
    };

    for (auto i = 0; i < graph.size(); ++i) {
        if (!visited[i]) {
            dfs(i);
        }
    }

    return out;
}

auto strongly_connected_components(graph_t const& graph,
                                   graph_t const& inverted_graph) {
    auto out = out_order(graph);

    vec<num> components(graph.size());
    auto curr_component = 0;

    vec<bool> visited(graph.size());

    std::function<void(num)> dfs;
    dfs = [&](num u) {
        visited[u] = true;
        components[u] = curr_component;

        for (auto& v : inverted_graph[u]) {
            if (!visited[v]) {
                dfs(v);
            }
        }
    };

    for (auto it = out.rbegin(); it != out.rend(); ++it) {
        if (!visited[*it]) {
            dfs(*it);

            ++curr_component;
        }
    }

    return std::make_pair(components, curr_component);
}

auto build_compressed_graph(graph_t const& graph,
                            vec<num> const& components,
                            num component_amt) {
    graph_t compressed_graph(component_amt);
    for (auto u = 0; u < graph.size(); ++u) {
        for (auto& v : graph[u]) {
            auto component_u = components[u];
            auto component_v = components[v];

            if (component_u != component_v) {
                compressed_graph[component_u].emplace_back(component_v);
            }
        }
    }

    return compressed_graph;
}

auto solve(graph_t&& graph,
           graph_t&& inverted_graph,
           vec<num> const& fun_values,
           num start_node,
           num end_node) {
    auto n = graph.size();

    auto [components, component_amt] =
        strongly_connected_components(graph, inverted_graph);

    auto compressed_graph =
        build_compressed_graph(graph, components, component_amt);

    graph.clear();
    inverted_graph.clear();

    vec<num> compressed_fun_values(compressed_graph.size());
    for (auto u = 0; u < n; ++u) {
        auto component_u = components[u];

        compressed_fun_values[component_u] += fun_values[u];
    }

    auto start_component = components[start_node];
    auto end_component = components[end_node];

    vec<num> dp(compressed_graph.size(), -1);

    std::function<num(num)> recurse;
    recurse = [&](num u) {
        auto& res = dp[u];
        if (res == -1) {
            if (u == end_component) {
                res = 0;
            } else {
                res = std::numeric_limits<num>::min();

                for (auto& v : compressed_graph[u]) {
                    auto curr_res = compressed_fun_values[v] + recurse(v);

                    res = std::max(res, curr_res);
                }
            }
        }

        return res;
    };

    return compressed_fun_values[start_component] + recurse(start_component);
}
}  // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    num n, m, s, e;
    std::cin >> n >> m >> s >> e;

    --s, --e;

    vec<num> fun_values(n);
    for (auto& val : fun_values) {
        std::cin >> val;
    }

    graph_t graph(n), inverted_graph(n);

    num a, b;
    while (m--) {
        std::cin >> a >> b;
        --a, --b;

        graph[a].emplace_back(b);
        inverted_graph[b].emplace_back(a);
    }

    std::cout << solve(std::move(graph), std::move(inverted_graph), fun_values,
                       s, e)
              << ENDL;

    return 0;
}

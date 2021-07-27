#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
template <class T>
using vec = std::vector<T>;

template <class T>
using mat = vec<vec<T>>;

template <class T>
auto multiply(mat<T> const& a, mat<T> const& b) {
    auto x1 = a.size(), y1 = a[0].size();
    auto x2 = b.size(), y2 = b[0].size();

    mat<T> res(x1, vec<T>(y2, T(0)));
    for (auto i = 0; i < x1; ++i) {
        for (auto j = 0; j < y2; ++j) {
            for (auto k = 0; k < y1; ++k) {
                res[i][j] += a[i][k] * b[k][j];
            }
        }
    }

    return res;
}

template <class T>
auto identity_matrix(size_t s) {
    mat<T> res(s, vec<T>(s, T(0)));
    for (auto i = 0; i < s; ++i) {
        res[i][i] = T(1);
    }

    return res;
}

template <class T>
auto exponentiate(mat<T> m, int p) {
    assert(m.size() == m[0].size());

    auto s = m.size();
    auto res = identity_matrix<T>(s);

    for (; p > 0; p >>= 1) {
        if (p & 1) {
            res = multiply(res, m);
        }

        m = multiply(m, m);
    }

    return res;
}

auto solve(int n, double p) {
    mat<double> multiplier = {{1 - p, p}, {p, 1 - p}};

    auto res = exponentiate(multiplier, n);

    return res[0][0];
}
}  // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    double p;

    std::cin >> n >> p;
    std::cout << std::setprecision(10) << std::fixed << solve(n, p) << ENDL;

    return 0;
}
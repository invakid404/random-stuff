#include <bits/stdc++.h>

namespace {
using num = long long;

const num MOD = 1e9 + 7;

template <class T>
using vec = std::vector<T>;

template <class T>
using mat = vec<vec<T>>;

template <class T>
mat<T> operator*(mat<T> const& a, mat<T> const& b) {
    int x1 = a.size(), y1 = a[0].size(), y2 = b[0].size();

    mat<T> res(x1, vec<T>(y2, T(0)));
    for (auto i = 0; i < x1; ++i) {
        for (auto j = 0; j < y2; ++j) {
            for (auto k = 0; k < y1; ++k) {
                res[i][j] += a[i][k] * b[k][j];
            }

            res[i][j] %= MOD;
        }
    }

    return res;
}

template <class T>
mat<T> operator%(mat<T> const& a, num) {
    return a;
}

template <class T>
T fast_exponentiation(T value, num pow) {
    auto res = value;
    --pow;

    for (; pow > 0; pow >>= 1) {
        if (pow & 1) {
            res = (res * value) % MOD;
        }

        value = (value * value) % MOD;
    }

    return res;
}

template <class T>
mat<T> operator^(mat<T> m, num p) {
    return fast_exponentiation(m, p);
}

auto solve_mat(num n, mat<num> const& initial, mat<num> const& transform) {
    auto res = initial * (transform ^ n);

    return res[0][0];
}

auto solve_1(num n) {
    return solve_mat(n + 1, {{0, 1}}, {{0, 1}, {1, 1}});
}

auto solve_2(num n) {
    if (n % 3 != 0) {
        return num(0);
    }

    return fast_exponentiation(num(2), n / 3);
}

auto solve_3(num n) {
    return solve_mat(n, {{1, 1, 2}}, {{0, 0, 1}, {1, 0, 0}, {0, 1, 2}});
}
}  // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    num n, t;
    std::cin >> n >> t;

    switch (t) {
    case 1:
        std::cout << solve_1(n) << std::endl;

        break;
    case 2:
        std::cout << solve_2(n) << std::endl;

        break;
    case 3:
        std::cout << solve_3(n) << std::endl;

        break;
    }

    return 0;
}

#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
using num = long long;

template <class T>
using vec = std::vector<T>;

template <class T>
using mat = vec<vec<T>>;

const num SIZE = 9;

template <class T>
mat<T> operator*(mat<T> const& a, mat<T> const& b) {
    int x1 = a.size(), y1 = a[0].size(), y2 = b[0].size();

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
T fast_exponentiation(T value, num pow) {
    auto res = value;
    --pow;

    for (; pow > 0; pow >>= 1) {
        if (pow & 1) {
            res = res * value;
        }

        value = value * value;
    }

    return res;
}

template <class T>
mat<T> operator^(mat<T> m, num p) {
    return fast_exponentiation(m, p);
}

auto read_input() {
    std::ifstream in("input.txt");

    vec<num> values;
    std::string data;

    while (std::getline(in, data, ',')) {
        values.push_back(std::stoi(data));
    }

    vec<num> count(SIZE);
    for (auto& val : values) {
        ++count[val];
    }

    return count;
}

mat<num> recurrence_matrix;

auto precompute_recurrence_matrix() {
    recurrence_matrix = mat<num>(SIZE, vec<num>(SIZE, 0));

    // Shift values to the left
    for (auto i = 0; i < SIZE - 1; ++i) {
        recurrence_matrix[i + 1][i] = 1;
    }

    // Create new lanternfish
    recurrence_matrix[0][6] = 1;
    recurrence_matrix[0][8] = 1;
}

auto solve(vec<num> const& data, num days) {
    mat<num> initial{data};
    auto final_state = initial * (recurrence_matrix ^ days);

    num res = 0;
    for (auto& val : final_state[0]) {
        res += val;
    }

    return res;
}
}  // namespace

int main() {
    auto input = read_input();

    precompute_recurrence_matrix();

    std::cout << solve(input, 80) << " " << solve(input, 256) << ENDL;

    return 0;
}

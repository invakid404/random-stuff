#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
const int mod = 1e9 + 7;

auto fast_pow(int n, int k) {
    auto res = 1;
    for (; k > 0; k >>= 1) {
        if (k & 1) {
            res = ((long long)res * n) % mod;
        }

        n = ((long long)n * n) % mod;
    }

    return res;
}
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    std::cin >> n;

    std::cout << fast_pow(2, n) << ENDL;

    return 0;
}
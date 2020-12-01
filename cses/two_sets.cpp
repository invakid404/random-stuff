#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
using num = unsigned long long;

auto solve(num n) {
    std::vector<num> s1, s2;

    auto total = (n * (n + 1)) / 2;
    if (total & 1) {
        return std::make_tuple(false, s1, s2);
    }

    auto target = total / 2;
    for (auto i = n; i >= 1; --i) {
        if (i <= target) {
            target -= i;
            s1.push_back(i);
        } else {
            s2.push_back(i);
        }
    }

    return std::make_tuple(true, s1, s2);
}
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    num n;
    std::cin >> n;

    auto [success, s1, s2] = solve(n);
    if (success) {
        std::cout << "YES" << ENDL;

        std::cout << s1.size() << ENDL;
        for (auto it = s1.rbegin(); it != s1.rend(); ++it) {
            std::cout << *it << " ";
        }
        std::cout << ENDL;

        std::cout << s2.size() << ENDL;
        for (auto it = s2.rbegin(); it != s2.rend(); ++it) {
            std::cout << *it << " ";
        }
        std::cout << ENDL;
    } else {
        std::cout << "NO" << ENDL;
    }

    return 0;
}
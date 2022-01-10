#include <bits/stdc++.h>
#include <ext/rope>

#define ENDL '\n'

namespace {
using num = int;

using __gnu_cxx::rope;
}  // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    num n, m;
    std::cin >> n >> m;

    rope<num> v;
    for (auto i = 1; i <= n; ++i) {
        v.push_back(i);
    }

    num l, r;
    while (m--) {
        std::cin >> l >> r;
        --l, --r;

        auto len = r - l + 1;

        auto curr = v.substr(l, len);
        v.erase(l, len);
        v.insert(v.mutable_begin(), curr);
    }

    for (auto val : v) {
        std::cout << val << " ";
    }
    std::cout << ENDL;

    return 0;
}

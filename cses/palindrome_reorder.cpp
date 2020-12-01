#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
// TODO: refactor this
auto solve(std::string& s) {
    std::vector<int> count(26);
    for (auto& c : s) {
        ++count[c - 'A'];
    }

    auto n = static_cast<int>(s.size());

    std::string res;

    if (n & 1) {
        auto char_amt = 0;
        auto evens = 0;
        for (auto& c : count) {
            if (c != 0) {
                ++char_amt;
                if (!(c & 1)) {
                    ++evens;
                }
            }
        }

        if (evens < char_amt - 1) {
            return res;
        }

        res.reserve(n);

        auto odd = -1;
        for (auto i = 0; i < 26; ++i) {
            if (count[i] & 1) {
                odd = i;
            }

            count[i] /= 2;
            res.append(count[i], 'A' + i);
        }

        if (odd != -1) {
            res.push_back('A' + odd);
        }

        for (auto i = 25; i >= 0; --i) {
            res.append(count[i], 'A' + i);
        }
    } else {
        for (auto& c : count) {
            if (c != 0 && c & 1) {
                return res;
            }
        }

        res.reserve(n);

        for (int i = 0; i < 26; ++i) {
            count[i] /= 2;
            res.append(count[i], 'A' + i);
        }

        for (auto i = 25; i >= 0; --i) {
            res.append(count[i], 'A' + i);
        }
    }

    return res;
}
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    std::string s;
    std::cin >> s;

    auto res = solve(s);
    if (res.empty()) {
        std::cout << "NO SOLUTION" << ENDL;
    } else {
        std::cout << res << ENDL;
    }

    return 0;
}
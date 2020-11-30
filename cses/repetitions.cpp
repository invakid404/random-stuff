#include <bits/stdc++.h>

#define ENDL '\n'

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    std::string s;
    std::cin >> s;

    auto best_len = 0;
    auto last_idx = 0;

    for (auto i = 1; i <= s.size(); ++i) {
        if (i < s.size() && s[last_idx] == s[i]) {
            continue;
        }

        best_len = std::max(best_len, i - last_idx);
        last_idx = i;
    }

    std::cout << best_len << ENDL;

    return 0;
}
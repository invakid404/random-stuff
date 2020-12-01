#include <bits/stdc++.h>

#define ENDL '\n'

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    std::string s;
    std::cin >> s;

    std::sort(s.begin(), s.end());

    std::set<std::string> res;
    do {
        res.insert(s);
    } while (std::next_permutation(s.begin(), s.end()));

    std::cout << res.size() << ENDL;
    for (auto& r : res) {
        std::cout << r << ENDL;
    }

    return 0;
}
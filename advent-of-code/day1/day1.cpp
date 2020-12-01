#include <bits/stdc++.h>

#define ENDL '\n'

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    std::ifstream is("input.txt");

    std::vector<int> vec;
    int foo;
    while (is >> foo) {
        vec.emplace_back(foo);
    }

    std::unordered_set<int> complements;

    auto res = -1;
    for (auto& val : vec) {
        auto complement = 2020 - val;
        if (complements.find(complement) != complements.end()) {
            res = val * complement;
            break;
        }

        complements.insert(val);
    }

    std::cout << res << ENDL;

    return 0;
}
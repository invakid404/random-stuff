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

    for (auto& val : vec) {
        auto complement = 2020 - val;
        if (complements.find(complement) != complements.end()) {
            std::cout << val * complement << ENDL;
            break;
        }

        complements.insert(val);
    }

    return 0;
}
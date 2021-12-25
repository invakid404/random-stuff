#include <absl/container/flat_hash_set.h>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#define ENDL '\n'

namespace {
using num = int;

using map_t = std::vector<std::string>;

auto parse_input() {
    std::ifstream in("input.txt");

    std::vector<std::string> map;
    std::string line;

    while (std::getline(in, line)) {
        map.push_back(line);
    }

    return map;
}

auto part_one(map_t map) {
    auto step = [&](map_t& map, char target, num dr, num dc) {
        auto rows = map.size(), cols = map[0].size();
    
        auto has_moved = false;
        map_t new_map(rows, std::string(cols, '.'));

        for (auto r = 0; r < rows; ++r) {
            for (auto c = 0; c < cols; ++c) {
                if (map[r][c] == '.') {
                    continue;
                }

                if (map[r][c] == target) {
                    auto new_r = (r + dr) % rows;
                    auto new_c = (c + dc) % cols;

                    if (map[new_r][new_c] == '.') {
                        new_map[new_r][new_c] = target;
                        has_moved = true;

                        continue;
                    }
                }
                
                new_map[r][c] = map[r][c];
            }
        }

        map.swap(new_map);

        return has_moved;
    };

    for (auto curr_step = 1;; ++curr_step) {
        auto step_one = step(map, '>', 0, 1);
        auto step_two = step(map, 'v', 1, 0);

        if (!step_one && !step_two) {
            return curr_step;
        }
    }

    assert(0 && "unreachable");
}
}  // namespace

int main() {
    auto map = parse_input();

    std::cout << part_one(map) << ENDL;
}

#include <absl/container/flat_hash_set.h>
#include <absl/strings/ascii.h>
#include <absl/strings/str_split.h>
#include <algorithm>
#include <chrono>
#include <fstream>
#include <numeric>
#include <optional>
#include <sstream>
#include <string>

#define ENDL '\n'

namespace {
using num = int;

template <class T>
using vec = std::vector<T>;

using point_t = std::array<num, 3>;
using scanner_t = vec<point_t>;

auto parse_input() {
    std::ifstream in("input.txt");

    std::stringstream buffer;
    buffer << in.rdbuf();

    auto data = absl::StripAsciiWhitespace(buffer.str());
    vec<std::string> scanner_strs = absl::StrSplit(data, "\n\n");

    vec<scanner_t> scanners;
    for (auto& scanner_str : scanner_strs) {
        vec<std::string> scanner_lines = absl::StrSplit(scanner_str, "\n");

        scanner_t curr_scanner;
        curr_scanner.reserve(scanner_lines.size() - 1);

        for (auto it = std::next(scanner_lines.begin());
             it != scanner_lines.end(); ++it) {
            vec<std::string> coords = absl::StrSplit(*it, ",");

            point_t curr_point;
            auto idx = 0;

            for (auto& coord : coords) {
                curr_point[idx++] = std::stoi(coord);
            }

            curr_scanner.push_back(std::move(curr_point));
        }

        scanners.push_back(std::move(curr_scanner));
    }

    return scanners;
}

auto rotate(point_t const& point, int rotation) -> point_t {
    auto [x, y, z] = point;

    switch (rotation) {
    case 0:
        return {x, y, z};
    case 1:
        return {x, z, -y};
    case 2:
        return {x, -y, -z};
    case 3:
        return {x, -z, y};
    case 4:
        return {y, x, -z};
    case 5:
        return {y, z, x};
    case 6:
        return {y, -x, z};
    case 7:
        return {y, -z, -x};
    case 8:
        return {z, x, y};
    case 9:
        return {z, y, -x};
    case 10:
        return {z, -x, -y};
    case 11:
        return {z, -y, x};
    case 12:
        return {-x, y, -z};
    case 13:
        return {-x, z, y};
    case 14:
        return {-x, -y, z};
    case 15:
        return {-x, -z, -y};
    case 16:
        return {-y, x, z};
    case 17:
        return {-y, z, -x};
    case 18:
        return {-y, -x, -z};
    case 19:
        return {-y, -z, x};
    case 20:
        return {-z, x, -y};
    case 21:
        return {-z, y, x};
    case 22:
        return {-z, -x, y};
    case 23:
        return {-z, -y, -x};
    default:
        assert(0 && "not reachable");
    }

    return {};
}

template <class Op>
auto transform(point_t const& a, point_t const& b) -> point_t {
    auto [x1, y1, z1] = a;
    auto [x2, y2, z2] = b;

    auto op = Op();

    return {op(x1, x2), op(y1, y2), op(z1, z2)};
}

auto cartesian_distance(point_t const& a, point_t const& b) -> point_t {
    return transform<std::minus<num>>(a, b);
}

auto translate(point_t const& a, point_t const& b) -> point_t {
    return transform<std::plus<num>>(a, b);
}

auto merge_scanner(absl::flat_hash_set<point_t>& acc, scanner_t& scanner)
    -> std::optional<point_t> {
    for (auto rotation = 0; rotation < 24; ++rotation) {
        scanner_t rotated_scanner(scanner.size());
        std::transform(
            scanner.begin(), scanner.end(), rotated_scanner.begin(),
            [&](auto const& point) { return rotate(point, rotation); });

        for (auto& acc_point : acc) {
            for (auto& rotated_point : rotated_scanner) {
                auto distance = cartesian_distance(acc_point, rotated_point);

                scanner_t translated_scanner(rotated_scanner.size());
                std::transform(rotated_scanner.begin(), rotated_scanner.end(),
                               translated_scanner.begin(),
                               [&](auto const& point) {
                                   return translate(point, distance);
                               });

                int contained = 0;
                for (auto& translated_point : translated_scanner) {
                    if (acc.contains(translated_point)) {
                        ++contained;
                    }
                }

                if (contained >= 12) {
                    acc.insert(translated_scanner.begin(),
                               translated_scanner.end());

                    return std::optional<point_t>{distance};
                }
            }
        }
    }

    return std::nullopt;
}

auto part_one(absl::flat_hash_set<point_t> const& acc,
              vec<point_t> const& distances) {
    return acc.size();
}

auto part_two(absl::flat_hash_set<point_t> const& acc,
              vec<point_t> const& distances) {
    auto max_val = 0;

    for (auto i = distances.begin(); i != distances.end(); ++i) {
        for (auto j = std::next(i); j != distances.end(); ++j) {
            auto delta = cartesian_distance(*i, *j);

            auto curr_val =
                std::accumulate(delta.begin(), delta.end(), 0,
                                [&](auto const& acc, auto const& val) {
                                    return acc + std::abs(val);
                                });

            max_val = std::max(max_val, curr_val);
        }
    }

    return max_val;
}
}  // namespace

int main() {
    auto scanners = parse_input();

    auto acc = absl::flat_hash_set<point_t>(scanners.front().begin(),
                                            scanners.front().end());
    scanners.erase(scanners.begin());

    vec<point_t> distances;
    while (!scanners.empty()) {
        for (auto it = scanners.begin(); it != scanners.end();) {
            auto distance_or_null = merge_scanner(acc, *it);

            if (distance_or_null.has_value()) {
                distances.push_back(distance_or_null.value());

                it = scanners.erase(it);
            } else {
                ++it;
            }
        }
    }

    std::cout << part_one(acc, distances) << " " << part_two(acc, distances)
              << ENDL;
}

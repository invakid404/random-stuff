#include <absl/container/flat_hash_map.h>
#include <array>
#include <cassert>
#include <fstream>
#include <iostream>
#include <optional>
#include <regex>
#include <tuple>

#define ENDL '\n'

namespace {
using num = long long;

struct vec3 {
    vec3(num x, num y, num z) : x(x), y(y), z(z) {}

    template <class Op>
    static auto bin_op(vec3 const& lhs, vec3 const& rhs, Op op) {
        return vec3(op(lhs.x, rhs.x), op(lhs.y, rhs.y), op(lhs.z, rhs.z));
    }

    friend auto operator+(vec3 const& lhs, vec3 const& rhs) {
        return vec3::bin_op(lhs, rhs, std::plus<num>());
    }

    friend auto operator-(vec3 const& lhs, vec3 const& rhs) {
        return vec3::bin_op(lhs, rhs, std::minus<num>());
    }

    friend auto operator*(vec3 const& lhs, vec3 const& rhs) {
        return vec3::bin_op(lhs, rhs, std::multiplies<num>());
    }

    static auto min(vec3 const& lhs, vec3 const& rhs) {
        return vec3::bin_op(lhs, rhs, [](auto const& a, auto const& b) {
            return std::min(a, b);
        });
    }

    static auto max(vec3 const& lhs, vec3 const& rhs) {
        return vec3::bin_op(lhs, rhs, [](auto const& a, auto const& b) {
            return std::max(a, b);
        });
    }

    friend auto operator<<(std::ostream& out, vec3 const& value)
        -> std::ostream& {
        return out << "(" << value.x << ", " << value.y << ", " << value.z
                   << ")";
    }

    friend bool operator==(vec3 const& lhs, vec3 const& rhs) {
        return lhs.x == rhs.x && lhs.y == rhs.y && lhs.z == rhs.z;
    }

    template <class Hash>
    friend auto AbslHashValue(Hash hash, vec3 const& value) -> Hash {
        return Hash::combine(std::move(hash), value.x, value.y, value.z);
    }

    num x, y, z;
};

struct cuboid_t {
    cuboid_t(vec3 min, vec3 max) : min(std::move(min)), max(std::move(max)) {}

    auto is_valid() {
        return this->min.x <= this->max.x && this->min.y <= this->max.y &&
               this->min.z <= this->max.z;
    }

    auto volume() const {
        auto value = this->max - this->min + vec3{1, 1, 1};
        return value.x * value.y * value.z;
    }

    friend auto operator-(cuboid_t const& lhs, cuboid_t const& rhs) {
        return cuboid_t(vec3::max(lhs.min, rhs.min),
                        vec3::min(lhs.max, rhs.max));
    }

    friend auto operator<<(std::ostream& out, cuboid_t const& lhs)
        -> std::ostream& {
        return out << lhs.min << ", " << lhs.max;
    }

    friend bool operator==(cuboid_t const& lhs, cuboid_t const& rhs) {
        return lhs.min == rhs.min && lhs.max == rhs.max;
    }

    template <class Hash>
    friend auto AbslHashValue(Hash hash, cuboid_t const& value) -> Hash {
        return Hash::combine(std::move(hash), value.min, value.max);
    }

    vec3 min, max;
};

using cuboids_t = std::vector<std::pair<cuboid_t, bool>>;

auto parse_input() {
    std::ifstream in("input.txt");

    static std::regex pattern(
        R"((on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+))",
        std::regex::optimize);

    std::string line;
    std::smatch matches;

    cuboids_t cuboids;

    while (std::getline(in, line)) {
        assert(std::regex_match(line, matches, pattern));

        std::array<num, 6> coords;

        std::transform(
            matches.begin() + 2, matches.end(), coords.begin(),
            [](auto const& group) { return std::stoi(group.str()); });

        auto min_vec = vec3(coords[0], coords[2], coords[4]);
        auto max_vec = vec3(coords[1], coords[3], coords[5]);

        auto cuboid = cuboid_t(min_vec, max_vec);

        cuboids.emplace_back(std::move(cuboid), matches[1].str() == "on");
    }

    return cuboids;
}

auto count_cuboids(cuboids_t const& cuboids) {
    absl::flat_hash_map<cuboid_t, num> all_cuboids;

    for (auto& [cuboid, is_on] : cuboids) {
        absl::flat_hash_map<cuboid_t, num> updated_cuboids;
        for (auto& [other_cuboid, other_value] : all_cuboids) {
            auto intersection = cuboid - other_cuboid;
            if (intersection.is_valid()) {
                updated_cuboids[intersection] -= other_value;
            }
        }

        if (is_on) {
            ++updated_cuboids[cuboid];
        }

        for (auto& [curr_cuboid, curr_value] : updated_cuboids) {
            all_cuboids[curr_cuboid] += curr_value;
        }
    }

    auto res = num();
    for (auto& [cuboid, value] : all_cuboids) {
        res += cuboid.volume() * value;
    }

    return res;
}

auto part_one(cuboids_t const& cuboids) {
    auto region = cuboid_t(vec3(-50, -50, -50), vec3(50, 50, 50));

    cuboids_t intersected_cuboids;
    for (auto& [cuboid, is_on] : cuboids) {
        auto intersection = cuboid - region;
        if (!intersection.is_valid()) {
            continue;
        }

        intersected_cuboids.emplace_back(std::move(intersection), is_on);
    }

    return count_cuboids(intersected_cuboids);
}

auto part_two(cuboids_t const& cuboids) {
    return count_cuboids(cuboids);
}
}  // namespace

int main() {
    auto cuboids = parse_input();

    std::cout << part_one(cuboids) << " " << part_two(cuboids) << ENDL;
}

cmake_minimum_required(VERSION 3.5)

project(advent-of-code)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_POSITION_INDEPENDENT_CODE TRUE)

find_package(absl REQUIRED)

add_executable(day19 day19.cpp)
add_executable(day22 day22.cpp)

target_link_libraries(day19 absl::flat_hash_set absl::strings)
target_link_libraries(day22 absl::flat_hash_map)

# frozen_string_literal: true

require 'json'

packets = File.foreach('day13.txt')
              .reject { |line| line.strip.empty? }
              .map(&JSON.method(:parse))

def zip_full(left, right)
  return left.zip(right) if left.length >= right.length

  right.zip(left).map(&:reverse)
end

def compare_lists(left, right)
  zip_full(left, right).each do |(left_element, right_element)|
    return -1 if left_element.nil?
    return 1 if right_element.nil?

    comparison = if left_element.is_a?(Array) || right_element.is_a?(Array)
                   compare_lists(Array(left_element), Array(right_element))
                 else
                   left_element <=> right_element
                 end

    return comparison if comparison != 0
  end

  0
end

part1 = packets
        .each_slice(2)
        .each_with_index
        .select { |pair, _index| compare_lists(*pair) <= 0 }
        .map { |_pair, index| index + 1 }
        .sum

divider_packets = [[[2]], [[6]]]

part2 = [*packets, *divider_packets]
        .sort(&method(:compare_lists))
        .each_with_index
        .select { |packet, _index| divider_packets.include?(packet) }
        .map { |_packet, index| index + 1 }
        .inject(:*)

puts part1, part2

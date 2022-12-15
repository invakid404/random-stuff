# frozen_string_literal: true

require 'set'

sensor_pattern = /Sensor at x=(?<x>-?\d+), y=(?<y>-?\d+): closest beacon is at x=(?<bx>-?\d+), y=(?<by>-?\d+)/

@sensors = File.foreach('day15.txt').map do |line|
  x, y, bx, by = sensor_pattern.match(line).captures.map(&:to_i)

  [x, y, bx, by, (x - bx).abs + (y - by).abs]
end

@row = 2_000_000

def part1
  ranges = @sensors.flat_map do |(x, y, bx, by, d)|
    rhs = d - (y - @row).abs
    next [] if rhs.negative?

    beacon_x1 = x - rhs
    beacon_x2 = x + rhs

    if by == @row && bx.between?(beacon_x1, beacon_x2)
      [[beacon_x1, bx - 1], [bx + 1, beacon_x2]].reject { |range| range.reverse.inject(:-).negative? }
    else
      [[beacon_x1, beacon_x2]]
    end
  end

  events = ranges.each_with_index.flat_map { |(x, y), idx| [[x, :x, idx], [y, :y, idx]] }.sort

  current_open = nil
  result = ranges.map { |range| range.reverse.inject(:-) + 1 }.sum

  events.each do |(_value, type, idx)|
    case type
    when :x
      if current_open.nil?
        current_open = idx

        next
      end

      result += ranges[idx].first - 1

      if ranges[idx].last > ranges[current_open].last
        result -= ranges[current_open].last
        current_open = idx

        next
      end

      result -= ranges[idx].last
    when :y
      current_open = nil if idx == current_open
    end
  end

  result
end

@max = 4_000_000

def part2
  @sensors.each do |(x, y, _bx, _by, d)|
    new_radius = d + 1
    (0..new_radius).each do |i|
      dx = new_radius - i
      dy = i

      result = [1, -1].repeated_permutation(2)
                      .map { |(fx, fy)| [x + dx * fx, y + dy * fy] }
                      .select { |point| point.all? { |coord| coord.between?(0, @max) } }
                      .find { |(px, py)| @sensors.all? { |(sx, sy, _bx, _by, sd)| (px - sx).abs + (py - sy).abs > sd } }

      return result.first * @max + result.last unless result.nil?
    end
  end
end

puts part1, part2

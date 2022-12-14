# frozen_string_literal: true

require 'set'

@blocked = Set.new

File.foreach('day14.txt')
    .map { |line| line.split(' -> ').map { |coord| coord.split(',').map(&:to_i) } }
    .each do |structure|
      structure.each_cons(2).map(&:sort).each do |(prev_x, prev_y), (curr_x, curr_y)|
        (prev_x..curr_x).each do |i|
          (prev_y..curr_y).each do |j|
            @blocked << [i, j]
          end
        end
      end
    end

@abyss = @blocked.map(&:last).max
@origin = [500, 0]

def solve(floor)
  blocked = @blocked.clone
  stack = [@origin]

  until stack.empty?
    node = stack.last

    return blocked.size - @blocked.size if blocked.include?(@origin) || (floor.nil? && node.last > @abyss)

    next_node = [0, -1, 1]
                .map { |dx| node.zip([dx, 1]).map(&:sum) }
                .find { |n| !blocked.include?(n) && n.last != floor }

    unless next_node.nil?
      stack << next_node

      next
    end

    blocked << node
    stack.pop
  end

  blocked.size - @blocked.size
end

puts solve(nil), solve(@abyss + 2)

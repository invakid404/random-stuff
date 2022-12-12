# frozen_string_literal: true

require 'matrix'

@grid = Matrix[*File.foreach('day12.txt').map(&:strip).map { |line| line.chars.map(&:ord) }]
start_pos = @grid.index('S'.ord)
end_pos = @grid.index('E'.ord)

@grid[*start_pos] = 'a'.ord
@grid[*end_pos] = 'z'.ord

def solve(start_pos, end_pos)
  queue = [end_pos]

  dist = Matrix.build(@grid.row_count, @grid.column_count) { -1 }
  dist[*end_pos] = 0

  until queue.empty?
    current_pos = queue.shift

    [
      [-1, +0],
      [+1, +0],
      [+0, -1],
      [+0, +1]
    ].each do |delta|
      next_pos = current_pos.zip(delta).map(&:sum)

      next if @grid[*next_pos].nil? ||
              @grid[*current_pos] - @grid[*next_pos] > 1 ||
              dist[*next_pos] != -1

      queue << next_pos
      dist[*next_pos] = dist[*current_pos] + 1
    end
  end

  part1 = dist[*start_pos]
  part2 = @grid.each_with_index
               .select { |value, *_pos| value == 'a'.ord }
               .map { |_value, *pos| dist[*pos] }
               .reject { |value| value == -1 }
               .min

  [part1, part2]
end

puts solve(start_pos, end_pos)

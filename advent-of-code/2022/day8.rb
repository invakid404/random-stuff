# frozen_string_literal: true

@grid = File.foreach('day8.txt').map { |line| line.strip.chars.map(&:to_i) }
@rows = @grid.length
@cols = @grid[0].length

def valid?(row, col)
  row >= 0 && row < @rows && col >= 0 && col < @cols
end

def solve(initial_value, iterate, mapper)
  visible = Array.new(@rows) { Array.new(@cols, initial_value) }

  (0..@rows).each do |row|
    iterate.call(visible, row, 0, [0, 1])
    iterate.call(visible, row, @cols - 1, [0, -1])
  end

  (0..@cols).each do |col|
    iterate.call(visible, 0, col, [1, 0])
    iterate.call(visible, @rows - 1, col, [-1, 0])
  end

  mapper.call(visible.transpose.map { |value| mapper.call(value) })
end

part1 = solve(
  0,
  lambda { |visible, row, col, (delta_x, delta_y)|
    max = -1
    while valid?(row, col)
      if @grid[row][col] > max
        visible[row][col] = 1
        max = @grid[row][col]
      end

      row += delta_x
      col += delta_y
    end
  },
  ->(value) { value.sum }
)

part2 = solve(
  1,
  lambda { |visible, row, col, (delta_x, delta_y)|
    stack = [[10, 0]]

    i = 0
    while valid?(row, col)
      stack.pop while stack[-1][0] < @grid[row][col]

      visible[row][col] *= i - stack[-1][1]
      stack.push([@grid[row][col], i])

      row += delta_x
      col += delta_y
      i += 1
    end
  },
  ->(value) { value.max }
)

puts part1, part2

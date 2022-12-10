# frozen_string_literal: true

require 'set'

@deltas = { 'L' => [-1, 0], 'R' => [1, 0], 'U' => [0, 1], 'D' => [0, -1] }

@operations = File.foreach('day09.txt').map(&:split)

def solve(length)
  chain = Array.new(length) { [0, 0] }
  seen = Set.new([chain.last])

  @operations.each do |(direction, steps)|
    (1..steps.to_i).each do
      delta = @deltas[direction]

      chain[0] = chain[0].zip(delta).map(&:sum)
      (1..length - 1).each do |idx|
        delta = chain[idx - 1]
                .zip(chain[idx].map(&:-@))
                .map(&:sum)

        next if delta.all? { |n| n.abs <= 1 }

        chain[idx] = chain[idx]
                     .zip(delta.map { |n| n <=> 0 })
                     .map(&:sum)
      end

      seen << chain.last
    end
  end

  seen.length
end

puts solve(2), solve(10)

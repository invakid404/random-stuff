# frozen_string_literal: true

@x = 1
cycles = 1

signal_strength = 0
image = []

operations = { 'noop' => [nil], 'addx' => [nil, ->(amount) { @x += amount }] }

File.foreach('day10.txt').map(&:split).each do |line|
  operation = operations[line.first]

  operation.each do |cycle|
    signal_strength += cycles * @x if ((cycles + 20) % 40).zero?
    pixel = (cycles - 1) % 40

    image.push((pixel - @x).abs <= 1 ? '#' : '.')

    cycle&.call(line.last.to_i)

    cycles += 1
  end
end

puts signal_strength
puts(image.each_slice(40).map(&:join))

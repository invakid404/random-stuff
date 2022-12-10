# frozen_string_literal: true

folder_sizes = Hash.new(0)

File.foreach('day07.txt').map(&:split).each_with_object([]) do |line, stack|
  case line
  in ['$', 'cd', '..']
    stack.pop
  in ['$', 'cd', folder]
    stack.push [stack.last, folder].compact.join('/')
  in [size, _] if size.match?(/^\d+$/)
    stack.each { |folder| folder_sizes[folder] += size.to_i }
  else
  end
end

puts folder_sizes.values.select { |value| value <= 100_000 }.sum
puts folder_sizes.values.select { |value| value >= folder_sizes['/'] - 40_000_000 }.min

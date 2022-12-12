# frozen_string_literal: true

require 'keisan'

cache = Keisan::AST::Cache.new
$calculator = Keisan::Calculator.new(cache: cache)

class Monkey
  def self.parse(data)
    monkey_regex = /Monkey\s(?<idx>\d+):\n
      \s*Starting\sitems:\s(?<items>[\d,\s]+)\n
      \s*Operation:\snew\s=\s(?<operation>[\w\s+*=]+)\n
      \s*Test:\sdivisible\sby\s(?<test>\d+)\n
      \s*If\strue:\sthrow\sto\smonkey\s(?<if_true>\d+)\n
      \s*If\sfalse:\sthrow\sto\smonkey\s(?<if_false>\d+)/x

    matches = monkey_regex.match(data)

    Monkey.new(
      matches[:items].split(', ').map(&:to_i),
      matches[:operation],
      matches[:test].to_i,
      matches[:if_true].to_i,
      matches[:if_false].to_i
    )
  end

  def initialize(*args)
    @items, @operation, @divisible_by, @if_true, @if_false = *args
    @inspected = 0
  end

  def add_item(item)
    @items.push(item)
  end

  def inspect_items(monkeys, worry_factor, lcm)
    @items.each do |worry_level|
      new_worry_level = $calculator.evaluate(@operation, old: worry_level) / worry_factor
      new_worry_level %= lcm

      @inspected += 1

      if (new_worry_level % @divisible_by).zero?
        monkeys[@if_true].add_item(new_worry_level)
      else
        monkeys[@if_false].add_item(new_worry_level)
      end
    end

    @items = []
  end

  attr_reader :inspected, :divisible_by
end

monkeys = File.read('day11.txt').split("\n\n").map { |data| Monkey.parse(data) }

@lcm = monkeys.map(&:divisible_by).reduce(1, :lcm)

def solve(monkeys, rounds, worry_factor)
  new_monkeys = Marshal.load(Marshal.dump(monkeys))

  rounds.times do
    new_monkeys.each do |monkey|
      monkey.inspect_items(new_monkeys, worry_factor, @lcm)
    end
  end

  new_monkeys.map(&:inspected).sort.last(2).inject(:*)
end

puts solve(monkeys, 20, 3)
puts solve(monkeys, 10_000, 1)


class Bot
  attr_accessor :low_to, :high_to, :chips

  def initialize
    @chips = []
  end
end

def give_chip(bots, target, value)
  bots[target] = Bot.new unless bots.key?(target)
  bots[target].chips << value
end

def min_max(a, b)
  [a, b].minmax
end

bots = {}
value_regex = /value (\d+) goes to (bot \d+)/
gives_regex = /(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/

File.foreach("input.txt") do |line|
  if value_regex.match?(line)
    value, bot_id = line.match(value_regex).captures
    bots[bot_id] = Bot.new unless bots.key?(bot_id)
    bots[bot_id].chips << value.to_i
  elsif gives_regex.match?(line)
    bot_id, low_to, high_to = line.match(gives_regex).captures
    bots[bot_id] = Bot.new unless bots.key?(bot_id)
    bots[bot_id].low_to = low_to
    bots[bot_id].high_to = high_to
  end
end

loop do
  action = false
  bots.each do |bot_id, b|
    if b.chips.size == 2
      action = true
      low, high = min_max(b.chips[0], b.chips[1])
      if low == 17 && high == 61
        puts bot_id
        exit
      end
      b.chips.clear

      give_chip(bots, b.low_to, low)
      give_chip(bots, b.high_to, high)
    end
  end
  break unless action
end

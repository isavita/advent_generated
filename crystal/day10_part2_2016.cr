
class Bot
  property low_to : String = ""
  property high_to : String = ""
  property chips : Array(Int32) = [] of Int32
end

def min_max(a : Int32, b : Int32) : {Int32, Int32}
  a < b ? {a, b} : {b, a}
end

def give_chip(bots : Hash(String, Bot), outputs : Hash(String, Int32), target : String, value : Int32)
  if target.starts_with?("bot")
    bots[target] ||= Bot.new
    bots[target].chips << value
  elsif target.starts_with?("output")
    outputs[target] = value
  end
end

def main
  bots = {} of String => Bot
  outputs = {} of String => Int32

  value_regex = /value (\d+) goes to (bot \d+)/
  gives_regex = /(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/

  File.each_line("input.txt") do |line|
    if match = line.match(value_regex)
      value = match[1].to_i
      bot_id = match[2]
      
      bots[bot_id] ||= Bot.new
      bots[bot_id].chips << value
    elsif match = line.match(gives_regex)
      bot_id = match[1]
      low_to = match[2]
      high_to = match[3]
      
      bots[bot_id] ||= Bot.new
      bots[bot_id].low_to = low_to
      bots[bot_id].high_to = high_to
    end
  end

  loop do
    action = false
    bots.each_value do |bot|
      if bot.chips.size == 2
        action = true
        low, high = min_max(bot.chips[0], bot.chips[1])
        bot.chips.clear

        give_chip(bots, outputs, bot.low_to, low)
        give_chip(bots, outputs, bot.high_to, high)
      end
    end
    break unless action
  end

  result = outputs["output 0"] * outputs["output 1"] * outputs["output 2"]
  puts result
end

main

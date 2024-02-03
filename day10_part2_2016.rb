
class Bot
  attr_accessor :low_to, :high_to, :chips

  def initialize
    @chips = []
  end
end

bots = {}
outputs = {}

File.foreach('input.txt') do |line|
  if line =~ /value (\d+) goes to (bot \d+)/
    value, bot_id = $1.to_i, $2
    bots[bot_id] ||= Bot.new
    bots[bot_id].chips << value
  elsif line =~ /(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/
    bot_id, low_to, high_to = $1, $2, $3
    bots[bot_id] ||= Bot.new
    bots[bot_id].low_to = low_to
    bots[bot_id].high_to = high_to
  end
end

loop do
  action = false
  bots.each do |_, bot|
    if bot.chips.size == 2
      action = true
      low, high = bot.chips.minmax
      bot.chips.clear

      if bot.low_to.start_with?("bot")
        bots[bot.low_to] ||= Bot.new
        bots[bot.low_to].chips << low
      else
        outputs[bot.low_to] = low
      end

      if bot.high_to.start_with?("bot")
        bots[bot.high_to] ||= Bot.new
        bots[bot.high_to].chips << high
      else
        outputs[bot.high_to] = high
      end
    end
  end
  break unless action
end

result = outputs["output 0"] * outputs["output 1"] * outputs["output 2"]
puts result

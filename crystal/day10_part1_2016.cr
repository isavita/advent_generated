class Bot
  property low_to : String
  property high_to : String
  property chips : Array(Int32)

  def initialize
    @low_to = ""
    @high_to = ""
    @chips = [] of Int32
  end
end

bots = {} of String => Bot
value_regex = /value (\d+) goes to (bot \d+)/
gives_regex = /(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/

File.open("input.txt") do |file|
  file.each_line do |line|
    if (match = value_regex.match(line))
      value = match[1].to_i
      bot_id = match[2].to_s
      bots[bot_id] ||= Bot.new
      bots[bot_id].chips << value
    elsif (match = gives_regex.match(line))
      bot_id, low_to, high_to = match[1].to_s, match[2].to_s, match[3].to_s
      bots[bot_id] ||= Bot.new
      bots[bot_id].low_to = low_to
      bots[bot_id].high_to = high_to
    end
  end
end

loop do
  action = false
  bots.each do |bot_id, bot|
    if bot.chips.size == 2
      action = true
      low, high = bot.chips.min, bot.chips.max
      if low == 17 && high == 61
        puts bot_id
        exit
      end
      bot.chips.clear
      (bots[bot.low_to] ||= Bot.new).chips << low
      (bots[bot.high_to] ||= Bot.new).chips << high
    end
  end
  break unless action
end

file = File.open("input.txt")
messages = file.gets_to_end.split("\n")
file.close

def get_original_message(messages)
  return "" if messages.empty?

  message_length = messages[0].size
  count = Array.new(message_length) { Hash(Char, Int32).new }

  messages.each do |message|
    message.each_char.with_index do |char, j|
      count[j][char] ||= 0
      count[j][char] += 1
    end
  end

  original_message = ""
  count.each do |char_count|
    original_message += char_count.min_by { |char, cnt| cnt }[0].to_s
  end

  original_message
end

def get_least_common_char(count)
  min_char = 'a'
  min_count = Int32.max

  count.each do |char, cnt|
    if cnt < min_count
      min_count = cnt
      min_char = char
    end
  end

  min_char
end

original_message = get_original_message(messages)
puts original_message

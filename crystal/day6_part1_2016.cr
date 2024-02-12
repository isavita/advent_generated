
messages = File.read("input.txt").lines

def get_corrected_message(messages)
  return "" if messages.empty?
  
  message_length = messages[0].size
  count = Array.new(message_length) { Hash(Char, Int32).new }

  messages.each do |message|
    message.each_char.with_index do |char, j|
      count[j][char] ||= 0
      count[j][char] += 1
    end
  end

  corrected_message = ""
  count.each do |char_count|
    corrected_message += get_most_common_char(char_count)
  end

  return corrected_message
end

def get_most_common_char(count)
  max_char = ' '
  max_count = 0

  count.each do |char, cnt|
    if cnt > max_count
      max_count = cnt
      max_char = char
    end
  end

  return max_char
end

corrected_message = get_corrected_message(messages)
puts corrected_message

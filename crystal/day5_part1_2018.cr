
def react(polymer : String) : String
  i = 0
  while i < polymer.size - 1
    if polymer[i] != polymer[i + 1] &&
       (polymer[i].ord - polymer[i + 1].ord).abs == 32
      return react("#{polymer[0, i]}#{polymer[i + 2, polymer.size - i - 2]}")
    end
    i += 1
  end
  return polymer
end

file = File.open("input.txt")
polymer = file.gets_to_end.strip

result = react(polymer)
puts result.size

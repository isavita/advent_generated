
polymer = File.read('input.txt').chomp

def react(polymer)
  reacted = true
  while reacted
    reacted = false
    i = 0
    while i < polymer.length - 1
      if polymer[i] != polymer[i + 1] && polymer[i].casecmp(polymer[i + 1]) == 0
        polymer.slice!(i, 2)
        reacted = true
      else
        i += 1
      end
    end
  end
  polymer.length
end

puts react(polymer)

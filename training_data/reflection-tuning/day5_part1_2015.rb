def nice_string?(str)
  vowels = str.scan(/[aeiou]/).count >= 3
  double_letter = str =~ /(.)\1/
  no_forbidden = str !~ /ab|cd|pq|xy/
  
  vowels && double_letter && no_forbidden
end

nice_count = File.readlines('input.txt').count { |line| nice_string?(line.strip) }

puts nice_count

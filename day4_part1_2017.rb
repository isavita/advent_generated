
puts File.readlines('input.txt').count { |line| line.split(' ').uniq.length == line.split(' ').length }

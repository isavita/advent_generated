
require 'set'

input = File.readlines('input.txt').map(&:chomp)

prerequisites = Hash.new { |h, k| h[k] = Set.new }
steps = Set.new

input.each do |line|
  step = line.match(/Step (\w) must be finished before step (\w) can begin/)[2]
  prerequisite = line.match(/Step (\w) must be finished before step (\w) can begin/)[1]
  prerequisites[step] << prerequisite
  steps.add(step)
  steps.add(prerequisite)
end

order = ''
while !steps.empty?
  available_steps = steps.select { |step| prerequisites[step].empty? }.sort
  next_step = available_steps.first
  order += next_step
  steps.delete(next_step)
  prerequisites.each { |_, prereqs| prereqs.delete(next_step) }
end

puts order

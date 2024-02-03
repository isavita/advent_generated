
# Read and parse input
input = File.read('input.txt').scan(/Step (\w) must be finished before step (\w) can begin./)
dependencies = Hash.new { |h, k| h[k] = [] }
input.each { |pre, step| dependencies[step] << pre; dependencies[pre] ||= [] }

# Part 1
order = ''
while dependencies.any?
  next_step = dependencies.select { |_, v| v.empty? }.keys.min
  order << next_step
  dependencies.delete(next_step)
  dependencies.each { |_, v| v.delete(next_step) }
end
puts order

# Part 2
dependencies = input.each_with_object(Hash.new { |h, k| h[k] = [] }) { |(pre, step), h| h[step] << pre; h[pre] ||= [] }
workers = Array.new(5) { [nil, 0] } # [task, time left]
time = 0
completed = ''

until dependencies.empty? && workers.all? { |task, _| task.nil? }
  workers.each_with_index do |(task, time_left), i|
    if task && time_left == 1
      completed << task
      dependencies.each { |_, v| v.delete(task) }
      workers[i] = [nil, 0]
    elsif task
      workers[i][1] -= 1
    end
  end

  available_steps = dependencies.select { |_, v| v.empty? }.keys.sort
  available_steps.each do |step|
    if worker = workers.find { |task, _| task.nil? }
      worker[0] = step
      worker[1] = step.ord - 'A'.ord + 61
      dependencies.delete(step)
    end
  end

  time += 1 unless workers.all? { |task, _| task.nil? }
end

puts time

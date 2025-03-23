
require "set"

def parse_input(filename : String)
  instructions = File.read_lines(filename)
  tasks = Hash(String, Array(String)).new { |h, k| h[k] = [] of String }
  indegree = Hash(String, Int32).new(0)

  instructions.each do |line|
    if (match = line.match(/Step (\w) must be finished before step (\w) can begin/))
      prereq = match[1]
      step = match[2]
      tasks[prereq] << step
      indegree[step] += 1
      indegree[prereq] ||= 0
    end
  end

  {tasks, indegree}
end

def task_duration(task : String, base_time : Int32 = 60) : Int32
  base_time + task[0].ord - 'A'.ord + 1
end

def total_time_to_complete(filename : String, worker_count : Int32 = 5, base_time : Int32 = 60) : Int32
  tasks, indegree = parse_input(filename)
  available = indegree.select { |_, degree| degree == 0 }.keys.sort.reverse
  in_progress = Hash(String, Int32).new
  time_elapsed = 0

  until available.empty? && in_progress.empty?
    while !available.empty? && in_progress.size < worker_count
      task = available.pop
      in_progress[task] = task_duration(task, base_time)
    end

    min_time_left = in_progress.values.min
    time_elapsed += min_time_left

    completed_tasks = [] of String
    in_progress.each do |task, time_left|
      in_progress[task] = time_left - min_time_left
      if in_progress[task] <= 0
        completed_tasks << task
      end
    end

    completed_tasks.each do |task|
      in_progress.delete(task)
    end

    completed_tasks.each do |task|
      tasks[task].each do |next_task|
        indegree[next_task] -= 1
        if indegree[next_task] == 0
          available << next_task
        end
      end
      available.sort!.reverse!
    end
  end

  time_elapsed
end

puts total_time_to_complete("input.txt")

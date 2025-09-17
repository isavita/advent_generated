defmodule Scheduler do
  def duration(task, base) do
    code = task |> String.to_charlist() |> hd
    base + (code - ?A) + 1
  end

  def insert_sorted(list, item) do
    Enum.sort([item | list])
  end

  def parse(lines) do
    Enum.reduce(lines, {%{}, %{}, MapSet.new()}, fn line, {tasks, indeg, all} ->
      case Regex.run(~r/Step (\w) must be finished before step (\w) can begin\./, line) do
        [_, prereq, step] ->
          tasks2 = Map.update(tasks, prereq, [step], fn l -> l ++ [step] end)
          indeg2 = Map.update(indeg, step, 1, fn v -> v + 1 end)
          all2 = all |> MapSet.put(prereq) |> MapSet.put(step)
          {tasks2, indeg2, all2}
        _ -> {tasks, indeg, all}
      end
    end)
  end

  def total_time_until_completion(lines, _worker_count, _base_time) do
    {tasks, indeg, all} = parse(lines)

    indeg = Enum.reduce(MapSet.to_list(all), indeg, fn t, acc ->
      if Map.has_key?(acc, t), do: acc, else: Map.put(acc, t, 0)
    end)

    available =
      indeg
      |> Enum.filter(fn {_k, v} -> v == 0 end)
      |> Enum.map(fn {k, _v} -> k end)
      |> Enum.sort()

    state = %{
      tasks: tasks,
      base: _base_time,
      worker: _worker_count,
      available: available,
      in_progress: %{},
      indegree: indeg,
      time: 0
    }

    loop(state)
  end

  def loop(state) do
    state = fill_up(state)
    if state.available == [] and map_size(state.in_progress) == 0 do
      state.time
    else
      min_time = state.in_progress |> Map.values() |> Enum.min()
      time = state.time + min_time

      {completed, inprog} =
        Enum.reduce(state.in_progress, {[], %{}}, fn {task, tleft}, {acc, acc_map} ->
          nt = tleft - min_time
          if nt <= 0, do: {[task | acc], acc_map}, else: {acc, Map.put(acc_map, task, nt)}
        end)

      {indeg2, avail2} =
        Enum.reduce(completed, {state.indegree, state.available}, fn task, {indeg, avail} ->
          if Map.has_key?(state.tasks, task) do
            Enum.reduce(state.tasks[task], {indeg, avail}, fn next_task, {i, a} ->
              deg = Map.get(i, next_task, 0) - 1
              i2 = Map.put(i, next_task, deg)
              if deg == 0, do: {i2, insert_sorted(a, next_task)}, else: {i2, a}
            end)
          else
            {indeg, avail}
          end
        end)

      state2 = %{
        tasks: state.tasks,
        base: state.base,
        worker: state.worker,
        available: avail2,
        in_progress: inprog,
        indegree: indeg2,
        time: time
      }

      loop(state2)
    end
  end

  def fill_up(state) do
    worker = state.worker
    base = state.base
    avail = state.available
    inprog = state.in_progress

    {avail2, inprog2} = fill_loop(avail, inprog, worker, base)
    %{state | available: avail2, in_progress: inprog2}
  end

  def fill_loop(avail, inprog, worker, base) do
    if avail != [] and map_size(inprog) < worker do
      [t | rest] = avail
      duration = Scheduler.duration(t, base)
      inprog2 = Map.put(inprog, t, duration)
      fill_loop(rest, inprog2, worker, base)
    else
      {avail, inprog}
    end
  end

  def main do
    input = File.read!("input.txt")
    lines = String.split(input, "\n", trim: true)
    ans = total_time_until_completion(lines, 5, 60)
    IO.puts(ans)
  end
end

Scheduler.main()
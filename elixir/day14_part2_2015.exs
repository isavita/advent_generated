
defmodule ReindeerRace do
  defstruct [:speed, :fly_time, :rest_time, distance: 0, points: 0, flying: true, time_in_mode: 0]

  def solve do
    reindeers = parse_input("input.txt")
    simulate_race_with_points(reindeers, 2503)
    |> Enum.max_by(& &1.points)
    |> Map.get(:points)
    |> IO.puts()
  end

  def parse_input(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      parts = String.split(line)
      %ReindeerRace{
        speed: String.to_integer(Enum.at(parts, 3)),
        fly_time: String.to_integer(Enum.at(parts, 6)),
        rest_time: String.to_integer(Enum.at(parts, 13))
      }
    end)
  end

  def simulate_race_with_points(reindeers, total_seconds) do
    Enum.reduce(1..total_seconds, reindeers, fn _, current_reindeers ->
      # Move reindeers
      moved_reindeers = Enum.map(current_reindeers, &move_reindeer/1)
      
      # Find max distance
      max_distance = Enum.max_by(moved_reindeers, & &1.distance).distance
      
      # Award points to reindeers at max distance
      Enum.map(moved_reindeers, fn reindeer ->
        if reindeer.distance == max_distance do
          %{reindeer | points: reindeer.points + 1}
        else
          reindeer
        end
      end)
    end)
  end

  defp move_reindeer(reindeer) do
    cond do
      reindeer.flying and reindeer.time_in_mode + 1 == reindeer.fly_time ->
        %{reindeer | 
          flying: false, 
          time_in_mode: 0, 
          distance: reindeer.distance + reindeer.speed
        }
      
      not reindeer.flying and reindeer.time_in_mode + 1 == reindeer.rest_time ->
        %{reindeer | 
          flying: true, 
          time_in_mode: 0, 
          distance: reindeer.distance
        }
      
      reindeer.flying ->
        %{reindeer | 
          distance: reindeer.distance + reindeer.speed, 
          time_in_mode: reindeer.time_in_mode + 1
        }
      
      true ->
        %{reindeer | time_in_mode: reindeer.time_in_mode + 1}
    end
  end
end

ReindeerRace.solve()

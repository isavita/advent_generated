
defmodule Main do
  def run do
    blueprints =
      File.stream!("input.txt")
      |> Stream.map(&String.trim/1)
      |> Enum.map(&parse_line/1)

    init = %{
      ore: 0,
      clay: 0,
      obsidian: 0,
      geode: 0,
      ore_robots: 1,
      clay_robots: 0,
      obsidian_robots: 0,
      geode_robots: 0,
      time_left: 32
    }

    prod =
      blueprints
      |> Enum.take(3)
      |> Enum.map(&max_geode(&1, init))
      |> Enum.reduce(1, &*/2)

    IO.puts(prod)
  end

  defp parse_line(line) do
    [_,
     id,
     ore,
     clay,
     obs_ore,
     obs_clay,
     geo_ore,
     geo_obs] =
      Regex.run(
        ~r/^Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\.$/,
        line
      )

    %{
      id: String.to_integer(id),
      ore_cost: String.to_integer(ore),
      clay: %{ore_cost: String.to_integer(clay)},
      obsidian: %{
        ore_cost: String.to_integer(obs_ore),
        clay_cost: String.to_integer(obs_clay)
      },
      geode: %{
        ore_cost: String.to_integer(geo_ore),
        obsidian_cost: String.to_integer(geo_obs)
      }
    }
  end

  defp max_geode(blueprint, init) do
    bfs(:queue.in(init, :queue.new()), MapSet.new(), 0, blueprint)
  end

  defp bfs(queue, visited, max, blueprint) do
    case :queue.out(queue) do
      {:empty, _} ->
        max

      {{:value, state}, q} ->
        max = if state.geode > max, do: state.geode, else: max

        if state.time_left == 0 do
          bfs(q, visited, max, blueprint)
        else
          o = Enum.max([
            blueprint.ore_cost,
            blueprint.clay.ore_cost,
            blueprint.obsidian.ore_cost,
            blueprint.geode.ore_cost
          ])

          state =
            state
            |> cap_robots(o, blueprint)
            |> cap_resources(blueprint)

          if MapSet.member?(visited, state) do
            bfs(q, visited, max, blueprint)
          else
            visited = MapSet.put(visited, state)

            q = :queue.in(
              %{
                ore: state.ore + state.ore_robots,
                clay: state.clay + state.clay_robots,
                obsidian: state.obsidian + state.obsidian_robots,
                geode: state.geode + state.geode_robots,
                ore_robots: state.ore_robots,
                clay_robots: state.clay_robots,
                obsidian_robots: state.obsidian_robots,
                geode_robots: state.geode_robots,
                time_left: state.time_left - 1
              },
              q
            )

            q =
              if state.ore >= blueprint.ore_cost do
                :queue.in(
                  %{
                    ore: state.ore - blueprint.ore_cost + state.ore_robots,
                    clay: state.clay + state.clay_robots,
                    obsidian: state.obsidian + state.obsidian_robots,
                    geode: state.geode + state.geode_robots,
                    ore_robots: state.ore_robots + 1,
                    clay_robots: state.clay_robots,
                    obsidian_robots: state.obsidian_robots,
                    geode_robots: state.geode_robots,
                    time_left: state.time_left - 1
                  },
                  q
                )
              else
                q
              end

            q =
              if state.ore >= blueprint.clay.ore_cost do
                :queue.in(
                  %{
                    ore: state.ore - blueprint.clay.ore_cost + state.ore_robots,
                    clay: state.clay + state.clay_robots,
                    obsidian: state.obsidian + state.obsidian_robots,
                    geode: state.geode + state.geode_robots,
                    ore_robots: state.ore_robots,
                    clay_robots: state.clay_robots + 1,
                    obsidian_robots: state.obsidian_robots,
                    geode_robots: state.geode_robots,
                    time_left: state.time_left - 1
                  },
                  q
                )
              else
                q
              end

            q =
              if state.ore >= blueprint.obsidian.ore_cost and state.clay >= blueprint.obsidian.clay_cost do
                :queue.in(
                  %{
                    ore: state.ore - blueprint.obsidian.ore_cost + state.ore_robots,
                    clay: state.clay - blueprint.obsidian.clay_cost + state.clay_robots,
                    obsidian: state.obsidian + state.obsidian_robots,
                    geode: state.geode + state.geode_robots,
                    ore_robots: state.ore_robots,
                    clay_robots: state.clay_robots,
                    obsidian_robots: state.obsidian_robots + 1,
                    geode_robots: state.geode_robots,
                    time_left: state.time_left - 1
                  },
                  q
                )
              else
                q
              end

            q =
              if state.ore >= blueprint.geode.ore_cost and state.obsidian >= blueprint.geode.obsidian_cost do
                :queue.in(
                  %{
                    ore: state.ore - blueprint.geode.ore_cost + state.ore_robots,
                    clay: state.clay + state.clay_robots,
                    obsidian: state.obsidian - blueprint.geode.obsidian_cost + state.obsidian_robots,
                    geode: state.geode + state.geode_robots,
                    ore_robots: state.ore_robots,
                    clay_robots: state.clay_robots,
                    obsidian_robots: state.obsidian_robots,
                    geode_robots: state.geode_robots + 1,
                    time_left: state.time_left - 1
                  },
                  q
                )
              else
                q
              end

            bfs(q, visited, max, blueprint)
          end
        end
    end
  end

  defp cap_robots(state, o, blueprint) do
    ore_robots = min(state.ore_robots, o)
    clay_robots = min(state.clay_robots, blueprint.obsidian.clay_cost)
    obsidian_robots = min(state.obsidian_robots, blueprint.geode.obsidian_cost)

    %{state |
      ore_robots: ore_robots,
      clay_robots: clay_robots,
      obsidian_robots: obsidian_robots}
  end

  defp cap_resources(state, blueprint) do
    max_ore =
      state.time_left *
        Enum.max([
          blueprint.ore_cost,
          blueprint.clay.ore_cost,
          blueprint.obsidian.ore_cost,
          blueprint.geode.ore_cost
        ]) -
        state.ore_robots * (state.time_left - 1)

    ore = min(state.ore, max_ore)

    max_clay = state.time_left * blueprint.obsidian.clay_cost - state.clay_robots * (state.time_left - 1)
    clay = min(state.clay, max_clay)

    max_obs = state.time_left * blueprint.geode.obsidian_cost - state.obsidian_robots * (state.time_left - 1)
    obsidian = min(state.obsidian, max_obs)

    %{state | ore: ore, clay: clay, obsidian: obsidian}
  end
end

Main.run()

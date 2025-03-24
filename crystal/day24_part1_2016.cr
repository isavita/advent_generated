
require "math"

module Solver
  extend self

  def cleaning_robot(input : String) : Int32
    grid = input.split("\n").map(&.to_s.chars)
    graph = [] of Array(Int32)

    grid.each_with_index do |row, r|
      row.each_with_index do |cell, c|
        if '0' <= cell <= '9'
          poi = cell
          distances_from_poi = bfs_get_edge_weights(grid, {r, c})

          if graph.empty?
            graph = Array.new(distances_from_poi.size) { [] of Int32 }
          end

          index = poi.to_i
          graph[index] = distances_from_poi
        end
      end
    end
    dfs(graph, 0, Set(Int32).new([0]), false)
  end

  record BfsNode, row : Int32, col : Int32, distance : Int32

  def bfs_get_edge_weights(grid, start)
    poi_to_distance = Hash(Char, Int32).new
    poi_to_distance[grid[start[0]][start[1]]] = 0
    queue = Deque(BfsNode).new
    queue.push(BfsNode.new(start[0], start[1], 0))
    visited = Set(Tuple(Int32, Int32)).new

    while (front = queue.shift?)
      next if visited.includes?({front.row, front.col})
      visited << {front.row, front.col}

      if '0' <= grid[front.row][front.col] <= '9'
        poi_to_distance[grid[front.row][front.col]] = front.distance
      end

      [{0, -1}, {0, 1}, {1, 0}, {-1, 0}].each do |d|
        next_row, next_col = front.row + d[0], front.col + d[1]

        if grid[next_row][next_col] != '#'
          queue.push(BfsNode.new(next_row, next_col, front.distance + 1))
        end
      end
    end

    distances = Array.new(poi_to_distance.size, 0)
    poi_to_distance.each do |num_str, dist|
      n = num_str.to_i
      distances[n] = dist
    end

    distances
  end

  def dfs(graph, entry_index, visited, return_to_zero)
    if graph.size == visited.size
      return return_to_zero ? graph[entry_index][0] : 0
    end

    min_distance = Int32::MAX
    graph[entry_index].each_with_index do |val, i|
      if !visited.includes?(i)
        visited << i
        dist = val + dfs(graph, i, visited, return_to_zero)
        min_distance = Math.min(min_distance, dist)
        visited.delete(i)
      end
    end
    min_distance
  end
end

def main
  input = File.read("input.txt").strip
  puts Solver.cleaning_robot(input)
end

main

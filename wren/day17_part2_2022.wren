
import "io" for File

class RockSim {
  static run() {
    var jet_pattern = File.read("input.txt").trim()
    var jet_len = jet_pattern.count
    var total_rocks = 1000000000000
    var profile_depth = 50

    var SHAPES = [
      [[0,0], [1,0], [2,0], [3,0]],
      [[1,0], [0,1], [1,1], [2,1], [1,2]],
      [[0,0], [1,0], [2,0], [2,1], [2,2]],
      [[0,0], [0,1], [0,2], [0,3]],
      [[0,0], [1,0], [0,1], [1,1]]
    ]

    var chamber = [127]
    var highest_y = 0

    var jet_index = 0
    var rock_index = 0
    var rock_number = 0
    var additional_height = 0

    var seen_states = {}

    var can_move = Fn.new { |dx, dy, rock_pts|
      for (pt in rock_pts) {
        var nx = pt[0] + dx
        var ny = pt[1] + dy
        if (nx < 0 || nx > 6 || ny <= 0) return false
        if (ny < chamber.count && (chamber[ny] & (1 << nx)) != 0) return false
      }
      return true
    }

    var get_profile = Fn.new { |h_y, depth|
      var profile = List.filled(7, 0)
      for (x in 0..6) {
        var found = false
        var limit = h_y - depth + 1
        if (limit < 1) limit = 1
        var y = h_y
        while (y >= limit) {
          if (y < chamber.count && (chamber[y] & (1 << x)) != 0) {
            profile[x] = h_y - y
            found = true
            break
          }
          y = y - 1
        }
        if (!found) {
          profile[x] = depth
        }
      }
      return profile.join(",")
    }

    while (rock_number < total_rocks) {
      var current_rock_type = rock_index % 5
      var shape = SHAPES[current_rock_type]

      var start_x = 2
      var start_y = highest_y + 4

      var rock_pts = shape.map { |pt| [start_x + pt[0], start_y + pt[1]] }.toList

      while (true) {
        var jet_char = jet_pattern[jet_index % jet_len]
        jet_index = jet_index + 1
        var push_dx = (jet_char == ">") ? 1 : -1

        if (can_move.call(push_dx, 0, rock_pts)) {
          for (pt in rock_pts) pt[0] = pt[0] + push_dx
        }

        if (can_move.call(0, -1, rock_pts)) {
          for (pt in rock_pts) pt[1] = pt[1] - 1
        } else {
          for (pt in rock_pts) {
            var x = pt[0]
            var y = pt[1]
            while (chamber.count <= y) {
              chamber.add(0)
            }
            chamber[y] = chamber[y] | (1 << x)
            if (y > highest_y) highest_y = y
          }
          break
        }
      }

      var profile_str = get_profile.call(highest_y, profile_depth)
      var state_key = "%(rock_index % 5),%(jet_index % jet_len),%(profile_str)"

      if (seen_states.containsKey(state_key)) {
        var prev_data = seen_states[state_key]
        var prev_rock = prev_data[0]
        var prev_height = prev_data[1]

        var cycle_len_rocks = rock_number - prev_rock
        var cycle_len_height = highest_y - prev_height

        if (cycle_len_rocks > 0) {
          var remaining_rocks = total_rocks - rock_number
          var num_cycles = (remaining_rocks / cycle_len_rocks).floor

          additional_height = additional_height + num_cycles * cycle_len_height
          rock_number = rock_number + num_cycles * cycle_len_rocks
          seen_states.clear()
        } else {
          seen_states[state_key] = [rock_number, highest_y]
        }
      } else {
        seen_states[state_key] = [rock_number, highest_y]
      }

      if (rock_number < total_rocks) {
        rock_number = rock_number + 1
      }
      rock_index = rock_index + 1
    }

    System.print(highest_y + additional_height)
  }
}

RockSim.run()

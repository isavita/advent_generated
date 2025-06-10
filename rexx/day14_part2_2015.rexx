
/* Rexx */
main:
  inputFile = 'input.txt'
  total_time = 2503
  reindeer_count = 0

  do while lines(inputFile) > 0
    line = linein(inputFile)
    reindeer_count = reindeer_count + 1
    i = reindeer_count
    reindeer.i.speed     = word(line, 4)
    reindeer.i.fly_time  = word(line, 7)
    reindeer.i.rest_time = word(line, 14)
  end
  call stream inputFile, 'C', 'CLOSE'

  /* Part 1 */
  max_dist = 0
  do i = 1 to reindeer_count
    s = reindeer.i.speed
    f = reindeer.i.fly_time
    r = reindeer.i.rest_time
    dist = get_distance(s, f, r, total_time)
    max_dist = max(max_dist, dist)
  end
  say max_dist

  /* Part 2 */
  points. = 0
  do t = 1 to total_time
    leading_dist = 0
    do i = 1 to reindeer_count
      s = reindeer.i.speed
      f = reindeer.i.fly_time
      r = reindeer.i.rest_time
      current_dist.i = get_distance(s, f, r, t)
      leading_dist = max(leading_dist, current_dist.i)
    end

    do i = 1 to reindeer_count
      if current_dist.i = leading_dist then
        points.i = points.i + 1
    end
  end

  max_points = 0
  do i = 1 to reindeer_count
    max_points = max(max_points, points.i)
  end
  say max_points

exit

get_distance: procedure
  parse arg speed, fly_time, rest_time, time
  cycle_time = fly_time + rest_time
  if cycle_time = 0 then return 0
  full_cycles = time % cycle_time
  remaining_time = time // cycle_time
  dist = full_cycles * fly_time * speed
  dist = dist + min(remaining_time, fly_time) * speed
  return dist

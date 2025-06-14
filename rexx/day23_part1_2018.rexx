
/* REXX */
main:
  nanobot_count = 0
  strongest_radius = -1
  strongest_idx = 0

  do i = 1 while lines('input.txt') > 0
    line = linein('input.txt')
    parse var line 'pos=<' nanobots.i.x ',' nanobots.i.y ',' nanobots.i.z '>, r=' nanobots.i.r .
    nanobot_count = i
    if nanobots.i.r > strongest_radius then do
      strongest_radius = nanobots.i.r
      strongest_idx = i
    end
  end

  sx = nanobots.strongest_idx.x
  sy = nanobots.strongest_idx.y
  sz = nanobots.strongest_idx.z

  in_range_count = 0
  do i = 1 to nanobot_count
    dist = abs(nanobots.i.x - sx) + abs(nanobots.i.y - sy) + abs(nanobots.i.z - sz)
    if dist <= strongest_radius then
      in_range_count = in_range_count + 1
  end

  say in_range_count
  return

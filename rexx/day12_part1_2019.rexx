
/* Rexx */
main:
  call init_moons
  do 1000
    call apply_gravity
    call apply_velocity
  end
  say total_energy()
exit

init_moons:
  moon.0 = 0
  do while lines('input.txt') > 0
    line = linein('input.txt')
    i = moon.0 + 1
    parse var line '<x=' moon.i.pos.x ',' 'y=' moon.i.pos.y ',' 'z=' moon.i.pos.z '>'
    moon.i.vel.x = 0
    moon.i.vel.y = 0
    moon.i.vel.z = 0
    moon.0 = i
  end
  call linein 'input.txt', 1, 0
return

apply_gravity:
  do i = 1 to moon.0
    do j = i + 1 to moon.0
      dx = sign(moon.j.pos.x - moon.i.pos.x)
      moon.i.vel.x = moon.i.vel.x + dx
      moon.j.vel.x = moon.j.vel.x - dx

      dy = sign(moon.j.pos.y - moon.i.pos.y)
      moon.i.vel.y = moon.i.vel.y + dy
      moon.j.vel.y = moon.j.vel.y - dy

      dz = sign(moon.j.pos.z - moon.i.pos.z)
      moon.i.vel.z = moon.i.vel.z + dz
      moon.j.vel.z = moon.j.vel.z - dz
    end
  end
return

apply_velocity:
  do i = 1 to moon.0
    moon.i.pos.x = moon.i.pos.x + moon.i.vel.x
    moon.i.pos.y = moon.i.pos.y + moon.i.vel.y
    moon.i.pos.z = moon.i.pos.z + moon.i.vel.z
  end
return

total_energy:
  total = 0
  do i = 1 to moon.0
    pot = abs(moon.i.pos.x) + abs(moon.i.pos.y) + abs(moon.i.pos.z)
    kin = abs(moon.i.vel.x) + abs(moon.i.vel.y) + abs(moon.i.vel.z)
    total = total + pot * kin
  end
return total

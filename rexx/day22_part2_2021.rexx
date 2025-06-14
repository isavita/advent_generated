
/* REXX */
call main
exit

main:
  numeric digits 30
  final.0 = 0
  filename = 'input.txt'

  call stream filename, 'c', 'open read'
  do while lines(filename) > 0
    line = linein(filename)
    parse var line state ' x=' x_range ',y=' y_range ',z=' z_range
    parse var x_range c.x1 '..' c.x2
    parse var y_range c.y1 '..' c.y2
    parse var z_range c.z1 '..' c.z2
    c.isOn = (state == 'on')

    toAdd.0 = 0
    do j = 1 to final.0
      ix1 = max(c.x1, final.j.x1)
      ix2 = min(c.x2, final.j.x2)
      iy1 = max(c.y1, final.j.y1)
      iy2 = min(c.y2, final.j.y2)
      iz1 = max(c.z1, final.j.z1)
      iz2 = min(c.z2, final.j.z2)

      if ix1 <= ix2 & iy1 <= iy2 & iz1 <= iz2 then do
        k = toAdd.0 + 1
        toAdd.k.isOn = 1 - final.j.isOn
        toAdd.k.x1 = ix1; toAdd.k.x2 = ix2
        toAdd.k.y1 = iy1; toAdd.k.y2 = iy2
        toAdd.k.z1 = iz1; toAdd.k.z2 = iz2
        toAdd.0 = k
      end
    end

    if c.isOn then do
      k = toAdd.0 + 1
      toAdd.k.isOn = c.isOn
      toAdd.k.x1 = c.x1; toAdd.k.x2 = c.x2
      toAdd.k.y1 = c.y1; toAdd.k.y2 = c.y2
      toAdd.k.z1 = c.z1; toAdd.k.z2 = c.z2
      toAdd.0 = k
    end

    do j = 1 to toAdd.0
      k = final.0 + j
      final.k.isOn = toAdd.j.isOn
      final.k.x1 = toAdd.j.x1; final.k.x2 = toAdd.j.x2
      final.k.y1 = toAdd.j.y1; final.k.y2 = toAdd.j.y2
      final.k.z1 = toAdd.j.z1; final.k.z2 = toAdd.j.z2
    end
    final.0 = final.0 + toAdd.0
  end
  call stream filename, 'c', 'close'

  total = 0
  do i = 1 to final.0
    vol = (final.i.x2 - final.i.x1 + 1) * ,
          (final.i.y2 - final.i.y1 + 1) * ,
          (final.i.z2 - final.i.z1 + 1)
    if final.i.isOn = 0 then
      vol = -vol
    total = total + vol
  end

  say total
  return

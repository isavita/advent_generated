
/* REXX */
call main
exit

main:
  firewall. = ''
  count = 0
  fname = 'input.txt'
  do while lines(fname) > 0
    count = count + 1
    parse value linein(fname) with firewall.count.d ':' firewall.count.r
    firewall.count.c = 2 * (strip(firewall.count.r) - 1)
  end
  firewall.0 = count
  call linein fname, 1, 0

  delay = 0
  do while caught(delay)
    delay = delay + 1
  end

  say delay
return

caught:
  procedure expose firewall.
  arg delay
  do i = 1 to firewall.0
    if (delay + firewall.i.d) // firewall.i.c = 0 then
      return 1
  end
  return 0

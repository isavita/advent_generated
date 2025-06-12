
/* REXX */
main:
  total_paper = 0
  total_ribbon = 0
  file = 'input.txt'

  do while LINES(file) > 0
    parse value LINEIN(file) with l 'x' w 'x' h
    s1 = l * w
    s2 = w * h
    s3 = h * l

    total_paper = total_paper + 2 * (s1 + s2 + s3) + MIN(s1, s2, s3)
    total_ribbon = total_ribbon + 2 * MIN(l+w, w+h, h+l) + (l*w*h)
  end

  say total_paper
  say total_ribbon
return

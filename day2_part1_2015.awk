
BEGIN {
  FS="x"
}

{
  l = $1
  w = $2
  h = $3

  sa = 2 * l * w + 2 * w * h + 2 * h * l
  slack = ((l * w < w * h) && (l * w < h * l) ? l * w : (w * h < h * l ? w * h : h * l))

  total = total + sa + slack
}

END {
  print total
}

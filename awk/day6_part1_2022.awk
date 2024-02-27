
{
  for (i = 1; i <= length($0) - 3; i++) {
    if (substr($0, i, 1) != substr($0, i + 1, 1) &&
        substr($0, i, 1) != substr($0, i + 2, 1) &&
        substr($0, i, 1) != substr($0, i + 3, 1) &&
        substr($0, i + 1, 1) != substr($0, i + 2, 1) &&
        substr($0, i + 1, 1) != substr($0, i + 3, 1) &&
        substr($0, i + 2, 1) != substr($0, i + 3, 1)) {
      print i + 3
      exit
    }
  }
}

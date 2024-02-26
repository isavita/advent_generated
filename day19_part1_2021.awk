
{
  for (i=1; i<=NF; i++) {
    split($i, coords, ",");
    x = coords[1];
    y = coords[2];
    z = coords[3];
    print x, y, z;
  }
}

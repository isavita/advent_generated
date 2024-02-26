
{
    fuel = int($1 / 3) - 2
    total += (fuel < 0 ? 0 : fuel)
}
END {
    print total
}

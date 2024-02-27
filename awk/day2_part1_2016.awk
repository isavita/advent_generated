
{
    for (i = 1; i <= length($0); i++) {
        if (substr($0, i, 1) == "U" && y > 1) y--;
        else if (substr($0, i, 1) == "D" && y < 3) y++;
        else if (substr($0, i, 1) == "L" && x > 1) x--;
        else if (substr($0, i, 1) == "R" && x < 3) x++;
    }
    printf "%d", (y - 1) * 3 + x;
}
END {
    print ""
}

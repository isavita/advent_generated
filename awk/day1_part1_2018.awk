
{
    freq += ($1 ~ /^-/) ? -substr($1, 2) : substr($1, 2)
}
END {
    print freq
}

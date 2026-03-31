
awk -F'-' 'NF==2 {
    low=$1+0; high=$2+0;
    if (low > high) { t=low; low=high; high=t }
    print low, high
}' input.txt | sort -n -k1,1 -k2,2 | awk '
BEGIN { total = 0; start = 0; end = -1; found = 0 }
{
    if (!found) {
        start = $1; end = $2; found = 1
    } else if ($1 <= end) {
        if ($2 > end) end = $2
    } else {
        total += (end - start + 1)
        start = $1; end = $2
    }
}
END {
    if (found) total += (end - start + 1)
    printf "Total fresh IDs: %d\n", total
}'

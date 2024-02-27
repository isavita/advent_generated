
{
    for (i = 1; i <= NF; i++) {
        arr[i, NR] = $i
    }
}
END {
    validTriangles = 0
    for (i = 1; i <= NF; i++) {
        for (j = 1; j <= NR; j += 3) {
            if (j+2 <= NR && (arr[i, j] + arr[i, j+1] > arr[i, j+2]) && (arr[i, j] + arr[i, j+2] > arr[i, j+1]) && (arr[i, j+1] + arr[i, j+2] > arr[i, j])) {
                validTriangles++
            }
        }
    }
    print validTriangles
}

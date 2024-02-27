
BEGIN {count=0} 
{
    split($1, arr, " "); 
    programs[arr[1]] = 1; 
    if ($2 != "") {
        count++; 
        split($2, subArr, ", "); 
        for (i in subArr) {
            above[subArr[i]] = 1
        }
    }
} 
END {
    for (program in programs) {
        if (!(program in above)) {
            print program
        }
    }
}

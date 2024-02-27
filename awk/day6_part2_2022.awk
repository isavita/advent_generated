
{
    s = $0
    n = 14
    for (i = n; i <= length(s); i++) {
        str = substr(s, i-n+1, n)
        split(str, arr, "")
        unique = 1
        for (j in arr) {
            if (++count[arr[j]] > 1) {
                unique = 0
                break
            }
        }
        delete count
        if (unique) {
            print i
            exit
        }
    }
    print -1
}

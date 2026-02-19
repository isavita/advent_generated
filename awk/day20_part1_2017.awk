
#!/usr/bin/awk -f

BEGIN {
    cnt = 0
    while ((getline line < "input.txt") > 0) {
        cnt++
        n = 0
        while (match(line, /-?[0-9]+/)) {
            n++
            num = substr(line, RSTART, RLENGTH) + 0
            nums[n] = num
            line = substr(line, RSTART + RLENGTH)
        }
        accel = 0; for (i = 7; i <= 9; i++) accel += (nums[i] >= 0 ? nums[i] : -nums[i])
        vel   = 0; for (i = 4; i <= 6; i++) vel   += (nums[i] >= 0 ? nums[i] : -nums[i])
        pos   = 0; for (i = 1; i <= 3; i++) pos   += (nums[i] >= 0 ? nums[i] : -nums[i])
        idx = cnt - 1
        if (cnt == 1 || accel < bestA || (accel == bestA && vel < bestV) ||
            (accel == bestA && vel == bestV && pos < bestP)) {
            bestA = accel; bestV = vel; bestP = pos; bestIdx = idx
        }
    }
    close("input.txt")
    print bestIdx
}

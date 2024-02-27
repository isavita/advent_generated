
BEGIN {
    FS=","
    while ((getline < "input.txt") > 0) {
        for (i = 1; i <= NF; i++) {
            lanternFishCounts[$i]++
        }
    }
    for (i = 1; i <= 256; i++) {
        newLanternFish = lanternFishCounts[0]
        for (j = 0; j < 8; j++) {
            lanternFishCounts[j] = lanternFishCounts[j+1]
        }
        lanternFishCounts[6] += newLanternFish
        lanternFishCounts[8] = newLanternFish
    }
    total = 0
    for (i = 0; i < 9; i++) {
        total += lanternFishCounts[i]
    }
    print total
}

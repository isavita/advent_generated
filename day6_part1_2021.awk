
#!/usr/bin/awk -f

BEGIN {
    totalFish = 0
    for (i = 0; i < 9; i++) {
        fishes[i] = 0
    }
    
    while ((getline < "input.txt") > 0) {
        split($0, fishStrs, ",")
        for (i in fishStrs) {
            fish = int(fishStrs[i])
            fishes[fish]++
        }
    }

    for (day = 1; day <= 80; day++) {
        newFish = fishes[0]
        for (i = 1; i < 9; i++) {
            fishes[i-1] = fishes[i]
        }
        fishes[6] += newFish
        fishes[8] = newFish
    }

    for (i = 0; i < 9; i++) {
        totalFish += fishes[i]
    }
    
    print totalFish
}

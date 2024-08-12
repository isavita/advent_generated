function sort(arr, n) {
    for (i = 1; i <= n; i++) {
        min_idx = i
        for (j = i + 1; j <= n; j++) {
            if (arr[j] < arr[min_idx]) {
                min_idx = j
            }
        }
        if (min_idx != i) {
            temp = arr[i]
            arr[i] = arr[min_idx]
            arr[min_idx] = temp
        }
    }
}

BEGIN {
    min_fuel = 2147483647
    count = 0
    while (getline < "input.txt") {
        split($0, numbers, ",")
        for (i in numbers) {
            positions[++count] = numbers[i]
        }
    }
    
    sort(positions, count)
    
    for (i = positions[1]; i <= positions[count]; i++) {
        fuel = 0
        for (j = 1; j <= count; j++) {
            diff = positions[j] - i
            if (diff < 0) diff = -diff
            fuel += (diff * (diff + 1)) / 2
        }
        if (fuel < min_fuel) min_fuel = fuel
    }
    print min_fuel
}
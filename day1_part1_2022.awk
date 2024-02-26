
BEGIN {
    maxCalories = 0
    currentCalories = 0
    while ((getline line < "input.txt") > 0) {
        if (line == "") {
            if (currentCalories > maxCalories) {
                maxCalories = currentCalories
            }
            currentCalories = 0
            continue
        }
        currentCalories += line
    }
    if (currentCalories > maxCalories) {
        maxCalories = currentCalories
    }
    print maxCalories
}

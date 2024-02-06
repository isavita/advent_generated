
#include <stdio.h>

int calculate_fuel(int mass) {
    return (mass / 3) - 2;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    int mass, total_fuel = 0;

    while (fscanf(file, "%d", &mass) != EOF) {
        total_fuel += calculate_fuel(mass);
    }

    fclose(file);

    printf("%d\n", total_fuel);

    return 0;
}

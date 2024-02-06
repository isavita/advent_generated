#include <stdio.h>

int calculate_fuel(int mass) {
    return mass / 3 - 2;
}

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    int mass, total_fuel = 0;

    while (fscanf(fp, "%d", &mass) != EOF) {
        int fuel = calculate_fuel(mass);
        total_fuel += fuel;

        while (fuel > 0) {
            fuel = calculate_fuel(fuel);
            if (fuel > 0) {
                total_fuel += fuel;
            }
        }
    }

    fclose(fp);

    printf("%d", total_fuel);

    return 0;
}
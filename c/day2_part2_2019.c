
#include <stdio.h>

int execute(int memory[], int size) {
    for (int i = 0; i < size; i += 4) {
        switch (memory[i]) {
            case 1:
                memory[memory[i+3]] = memory[memory[i+1]] + memory[memory[i+2]];
                break;
            case 2:
                memory[memory[i+3]] = memory[memory[i+1]] * memory[memory[i+2]];
                break;
            case 99:
                return memory[0];
        }
    }
    return memory[0];
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    int original[1000];
    int i = 0;
    while (fscanf(file, "%d,", &original[i]) != EOF) {
        i++;
    }
    fclose(file);

    for (int noun = 0; noun <= 99; noun++) {
        for (int verb = 0; verb <= 99; verb++) {
            int memory[1000];
            for (int j = 0; j < i; j++) {
                memory[j] = original[j];
            }
            memory[1] = noun;
            memory[2] = verb;
            if (execute(memory, i) == 19690720) {
                printf("%d\n", 100 * noun + verb);
                return 0;
            }
        }
    }

    return 0;
}

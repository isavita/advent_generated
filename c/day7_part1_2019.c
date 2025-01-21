
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int code[10000];
int code_size = 0;

int get_param(int address, int immediate) {
    int param = code[address];
    if (immediate) {
        return param;
    }
    return code[param];
}

int run(int input[], int input_size) {
    int ip = 0;
    int input_idx = 0;
    int output = 0;

    while (1) {
        int cmd = code[ip];
        int opcode = cmd % 100;

        switch (opcode) {
            case 1: {
                int param1 = get_param(ip + 1, cmd / 100 % 10);
                int param2 = get_param(ip + 2, cmd / 1000 % 10);
                int address = get_param(ip + 3, 1);
                code[address] = param1 + param2;
                ip += 4;
                break;
            }
            case 2: {
                int param1 = get_param(ip + 1, cmd / 100 % 10);
                int param2 = get_param(ip + 2, cmd / 1000 % 10);
                int address = get_param(ip + 3, 1);
                code[address] = param1 * param2;
                ip += 4;
                break;
            }
            case 3: {
                int address = get_param(ip + 1, 1);
                code[address] = input[input_idx++];
                ip += 2;
                break;
            }
            case 4: {
                int param1 = get_param(ip + 1, cmd / 100 % 10);
                output = param1;
                ip += 2;
                break;
            }
            case 5: {
                int param1 = get_param(ip + 1, cmd / 100 % 10);
                int param2 = get_param(ip + 2, cmd / 1000 % 10);
                if (param1 != 0) {
                    ip = param2;
                } else {
                    ip += 3;
                }
                break;
            }
            case 6: {
                int param1 = get_param(ip + 1, cmd / 100 % 10);
                int param2 = get_param(ip + 2, cmd / 1000 % 10);
                if (param1 == 0) {
                    ip = param2;
                } else {
                    ip += 3;
                }
                break;
            }
            case 7: {
                int param1 = get_param(ip + 1, cmd / 100 % 10);
                int param2 = get_param(ip + 2, cmd / 1000 % 10);
                int address = get_param(ip + 3, 1);
                code[address] = (param1 < param2) ? 1 : 0;
                ip += 4;
                break;
            }
            case 8: {
                int param1 = get_param(ip + 1, cmd / 100 % 10);
                int param2 = get_param(ip + 2, cmd / 1000 % 10);
                int address = get_param(ip + 3, 1);
                code[address] = (param1 == param2) ? 1 : 0;
                ip += 4;
                break;
            }
            case 99: {
                return output;
            }
            default: {
                printf("Error: Invalid opcode %d\n", opcode);
                exit(1);
            }
        }
    }
}

void load(const char *filename) {
    FILE *file = fopen(filename, "r");
    char line[10000];
    fgets(line, sizeof(line), file);
    fclose(file);

    char *token = strtok(line, ",");
    while (token != NULL) {
        code[code_size++] = atoi(token);
        token = strtok(NULL, ",");
    }
}

void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

int max_output = 0;

void permutations(int arr[], int n, int original_code[]) {
    if (n == 1) {
        int output = 0;
        for (int i = 0; i < 5; i++) {
            memcpy(code, original_code, code_size * sizeof(int));
            int input[] = {arr[i], output};
            output = run(input, 2);
        }
        if (output > max_output) {
            max_output = output;
        }
    } else {
        for (int i = 0; i < n; i++) {
            permutations(arr, n - 1, original_code);
            if (n % 2 == 1) {
                swap(&arr[i], &arr[n - 1]);
            } else {
                swap(&arr[0], &arr[n - 1]);
            }
        }
    }
}

int main() {
    load("input.txt");
    int original_code[code_size];
    memcpy(original_code, code, code_size * sizeof(int));
    int arr[] = {0, 1, 2, 3, 4};
    permutations(arr, 5, original_code);
    printf("%d\n", max_output);
    return 0;
}


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int *code;
    int ip;
    int input[2];
    int output;
    int relative_base;
} VM;

void load_code(VM *vm, const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *buffer = (char *)malloc(length + 1);
    fread(buffer, 1, length, file);
    fclose(file);

    buffer[length] = '\0';

    char *token = strtok(buffer, ",");
    int index = 0;
    while (token != NULL) {
        vm->code[index++] = atoi(token);
        token = strtok(NULL, ",");
    }

    free(buffer);
}

int get_param_address(VM *vm, int pos, int mode) {
    switch (mode) {
        case 0: return vm->code[pos];
        case 1: return pos;
        case 2: return vm->relative_base + vm->code[pos];
        default: exit(EXIT_FAILURE);
    }
}

void run_vm(VM *vm) {
    int arity, cmd, op_code, modes[3], params[3];

    while (1) {
        cmd = vm->code[vm->ip];
        op_code = cmd % 100;
        modes[0] = (cmd / 100) % 10;
        modes[1] = (cmd / 1000) % 10;
        modes[2] = (cmd / 10000) % 10;

        switch (op_code) {
            case 1:
                arity = 3;
                params[0] = get_param_address(vm, vm->ip + 1, modes[0]);
                params[1] = get_param_address(vm, vm->ip + 2, modes[1]);
                params[2] = get_param_address(vm, vm->ip + 3, modes[2]);
                vm->code[params[2]] = vm->code[params[0]] + vm->code[params[1]];
                break;
            case 2:
                arity = 3;
                params[0] = get_param_address(vm, vm->ip + 1, modes[0]);
                params[1] = get_param_address(vm, vm->ip + 2, modes[1]);
                params[2] = get_param_address(vm, vm->ip + 3, modes[2]);
                vm->code[params[2]] = vm->code[params[0]] * vm->code[params[1]];
                break;
            case 3:
                arity = 1;
                params[0] = get_param_address(vm, vm->ip + 1, modes[0]);
                vm->code[params[0]] = vm->input[0];
                vm->input[0] = vm->input[1];
                break;
            case 4:
                arity = 1;
                params[0] = get_param_address(vm, vm->ip + 1, modes[0]);
                vm->output = vm->code[params[0]];
                return;
            case 5:
                arity = 2;
                params[0] = get_param_address(vm, vm->ip + 1, modes[0]);
                params[1] = get_param_address(vm, vm->ip + 2, modes[1]);
                if (vm->code[params[0]] != 0) {
                    vm->ip = vm->code[params[1]];
                    continue;
                }
                break;
            case 6:
                arity = 2;
                params[0] = get_param_address(vm, vm->ip + 1, modes[0]);
                params[1] = get_param_address(vm, vm->ip + 2, modes[1]);
                if (vm->code[params[0]] == 0) {
                    vm->ip = vm->code[params[1]];
                    continue;
                }
                break;
            case 7:
                arity = 3;
                params[0] = get_param_address(vm, vm->ip + 1, modes[0]);
                params[1] = get_param_address(vm, vm->ip + 2, modes[1]);
                params[2] = get_param_address(vm, vm->ip + 3, modes[2]);
                vm->code[params[2]] = (vm->code[params[0]] < vm->code[params[1]]) ? 1 : 0;
                break;
            case 8:
                arity = 3;
                params[0] = get_param_address(vm, vm->ip + 1, modes[0]);
                params[1] = get_param_address(vm, vm->ip + 2, modes[1]);
                params[2] = get_param_address(vm, vm->ip + 3, modes[2]);
                vm->code[params[2]] = (vm->code[params[0]] == vm->code[params[1]]) ? 1 : 0;
                break;
            case 9:
                arity = 1;
                params[0] = get_param_address(vm, vm->ip + 1, modes[0]);
                vm->relative_base += vm->code[params[0]];
                break;
            case 99:
                return;
            default:
                exit(EXIT_FAILURE);
        }

        vm->ip += arity + 1;
    }
}

int beam(int x, int y) {
    VM vm;
    vm.code = (int *)malloc(10000 * sizeof(int));
    vm.ip = 0;
    vm.relative_base = 0;
    vm.input[0] = x;
    vm.input[1] = y;

    load_code(&vm, "input.txt");
    run_vm(&vm);

    int result = vm.output;
    free(vm.code);
    return result == 1;
}

int main() {
    int sum = 0;
    for (int y = 0; y < 50; y++) {
        for (int x = 0; x < 50; x++) {
            if (beam(x, y)) {
                sum++;
            }
        }
    }

    printf("%d\n", sum);
    return 0;
}

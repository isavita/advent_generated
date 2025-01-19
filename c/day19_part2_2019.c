
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MEM_SIZE 2048
#define INPUT_SIZE 16
#define OUTPUT_SIZE 16

typedef struct {
    long long code[MEM_SIZE];
    long long ip;
    long long relativeBase;
    long long input[INPUT_SIZE];
    int input_head;
    int input_tail;
    long long output[OUTPUT_SIZE];
    int output_head;
    int output_tail;
} VM;

void vm_init(VM *vm) {
    memset(vm->code, 0, sizeof(vm->code));
    vm->ip = 0;
    vm->relativeBase = 0;
    vm->input_head = 0;
    vm->input_tail = 0;
    vm->output_head = 0;
    vm->output_tail = 0;
}

void vm_load(VM *vm, const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    
    if ((read = getline(&line, &len, fp)) != -1) {
        char *token = strtok(line, ",");
        int i = 0;
        while (token != NULL && i < MEM_SIZE) {
            vm->code[i++] = atoll(token);
            token = strtok(NULL, ",");
        }
    }
    
    if (line) free(line);
    fclose(fp);
}

int vm_has_input(VM *vm) {
    return vm->input_head != vm->input_tail;
}

void vm_add_input(VM *vm, long long val) {
    vm->input[vm->input_tail] = val;
    vm->input_tail = (vm->input_tail + 1) % INPUT_SIZE;
    if(vm->input_tail == vm->input_head) {
        vm->input_head = (vm->input_head + 1) % INPUT_SIZE;
    }
}

int vm_has_output(VM *vm) {
    return vm->output_head != vm->output_tail;
}

long long vm_get_output(VM *vm) {
  long long out = vm->output[vm->output_head];
  vm->output_head = (vm->output_head + 1) % OUTPUT_SIZE;
  return out;
}


void vm_add_output(VM *vm, long long val) {
    vm->output[vm->output_tail] = val;
    vm->output_tail = (vm->output_tail + 1) % OUTPUT_SIZE;
    if (vm->output_tail == vm->output_head) {
        vm->output_head = (vm->output_head + 1) % OUTPUT_SIZE;
    }
}


long long vm_get_param(VM *vm, long long pos, int mode) {
    if (pos >= MEM_SIZE) {
         printf("Error: memory access out of bounds at pos %lld\n", pos);
        exit(EXIT_FAILURE);
    }
    
    long long value;
    switch (mode) {
        case 0:
            if(vm->code[pos] >= MEM_SIZE){
              printf("Error: memory access out of bounds at index %lld\n", vm->code[pos]);
              exit(EXIT_FAILURE);
            }
            value = vm->code[vm->code[pos]];
            break;
        case 1:
            value = vm->code[pos];
            break;
        case 2:
            if(vm->code[pos] + vm->relativeBase >= MEM_SIZE) {
              printf("Error: memory access out of bounds at index %lld\n", vm->code[pos] + vm->relativeBase);
              exit(EXIT_FAILURE);
            }
            value = vm->code[vm->code[pos] + vm->relativeBase];
            break;
         default:
            printf("Error: invalid parameter mode: %d\n", mode);
           exit(EXIT_FAILURE);
    }
    return value;
}


long long vm_get_param_addr(VM *vm, long long pos, int mode) {
    if (pos >= MEM_SIZE) {
      printf("Error: memory access out of bounds at pos %lld\n", pos);
        exit(EXIT_FAILURE);
    }

    long long addr;
    switch (mode) {
        case 0:
            addr = vm->code[pos];
            break;
        case 1:
             printf("Error: writing to immediate mode at pos %lld\n", pos);
              exit(EXIT_FAILURE);
             break;
        case 2:
            addr = vm->code[pos] + vm->relativeBase;
            break;
        default:
            printf("Error: invalid parameter mode: %d\n", mode);
            exit(EXIT_FAILURE);
    }
    return addr;
}

void vm_run(VM *vm) {
    long long opcode, param1, param2, param3, addr;
    int mode1, mode2, mode3;
    
    while (1) {
        opcode = vm->code[vm->ip] % 100;
        mode1 = (vm->code[vm->ip] / 100) % 10;
        mode2 = (vm->code[vm->ip] / 1000) % 10;
        mode3 = (vm->code[vm->ip] / 10000) % 10;


        switch (opcode) {
            case 1: // add
                param1 = vm_get_param(vm, vm->ip + 1, mode1);
                param2 = vm_get_param(vm, vm->ip + 2, mode2);
                addr = vm_get_param_addr(vm, vm->ip + 3, mode3);
                if (addr < 0 || addr >= MEM_SIZE){
                   printf("Error: memory access out of bounds, writing to address %lld\n", addr);
                   exit(EXIT_FAILURE);
                }
                vm->code[addr] = param1 + param2;
                vm->ip += 4;
                break;
            case 2: // multiply
                param1 = vm_get_param(vm, vm->ip + 1, mode1);
                param2 = vm_get_param(vm, vm->ip + 2, mode2);
                addr = vm_get_param_addr(vm, vm->ip + 3, mode3);
               if (addr < 0 || addr >= MEM_SIZE){
                   printf("Error: memory access out of bounds, writing to address %lld\n", addr);
                   exit(EXIT_FAILURE);
                }
                vm->code[addr] = param1 * param2;
                vm->ip += 4;
                break;
             case 3: // input
                if(!vm_has_input(vm)) {
                  return;
                 }
                addr = vm_get_param_addr(vm, vm->ip + 1, mode1);
               if (addr < 0 || addr >= MEM_SIZE){
                   printf("Error: memory access out of bounds, writing to address %lld\n", addr);
                   exit(EXIT_FAILURE);
                }
                vm->code[addr] = vm->input[vm->input_head];
                vm->input_head = (vm->input_head + 1) % INPUT_SIZE;
                vm->ip += 2;
                break;
            case 4: // output
               
                param1 = vm_get_param(vm, vm->ip + 1, mode1);
                vm_add_output(vm, param1);
                vm->ip += 2;
                break;
            case 5: // jump-if-true
                param1 = vm_get_param(vm, vm->ip + 1, mode1);
                param2 = vm_get_param(vm, vm->ip + 2, mode2);
                if (param1 != 0) {
                    vm->ip = param2;
                } else {
                    vm->ip += 3;
                }
                break;
            case 6: // jump-if-false
                param1 = vm_get_param(vm, vm->ip + 1, mode1);
                param2 = vm_get_param(vm, vm->ip + 2, mode2);
                if (param1 == 0) {
                    vm->ip = param2;
                } else {
                    vm->ip += 3;
                }
                break;
             case 7: // less than
                param1 = vm_get_param(vm, vm->ip + 1, mode1);
                param2 = vm_get_param(vm, vm->ip + 2, mode2);
                addr = vm_get_param_addr(vm, vm->ip + 3, mode3);
               if (addr < 0 || addr >= MEM_SIZE){
                   printf("Error: memory access out of bounds, writing to address %lld\n", addr);
                   exit(EXIT_FAILURE);
                }
                vm->code[addr] = (param1 < param2) ? 1 : 0;
                vm->ip += 4;
                break;
            case 8: // equals
                param1 = vm_get_param(vm, vm->ip + 1, mode1);
                param2 = vm_get_param(vm, vm->ip + 2, mode2);
                addr = vm_get_param_addr(vm, vm->ip + 3, mode3);
                if (addr < 0 || addr >= MEM_SIZE){
                   printf("Error: memory access out of bounds, writing to address %lld\n", addr);
                   exit(EXIT_FAILURE);
                }
                vm->code[addr] = (param1 == param2) ? 1 : 0;
                vm->ip += 4;
                break;
            case 9: // adjust relative base
               
                param1 = vm_get_param(vm, vm->ip + 1, mode1);
                vm->relativeBase += param1;
                vm->ip += 2;
                break;
            case 99:
                return;
             default:
                printf("Error: unknown opcode: %lld at position %lld\n", opcode, vm->ip);
                exit(EXIT_FAILURE);
        }
    }
}

bool beam(int x, int y) {
    VM vm;
    vm_init(&vm);
    vm_load(&vm, "input.txt");
    
    vm_add_input(&vm, x);
    vm_add_input(&vm, y);

    vm_run(&vm);
     if(!vm_has_output(&vm)){
       return false;
     }

    return vm_get_output(&vm) == 1;
}

int main() {
    int y = 20;
    int x = 0;

    while (1) {
        if (!beam(x, y)) {
            x++;
            continue;
        }

        if (!beam(x + 99, y)) {
            y++;
            continue;
        }

        if (!beam(x, y + 99)) {
            x++;
            continue;
        }
        
       printf("%d\n", x * 10000 + y);
       return 0;
    }
   return 0;
}

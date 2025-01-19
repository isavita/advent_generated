
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

#define MEM_SIZE 16384
#define INPUT_BUFFER_SIZE 256
#define OUTPUT_BUFFER_SIZE 256

typedef struct {
    int64_t code[MEM_SIZE];
    int64_t ip;
    int64_t relativeBase;
    int64_t inputBuffer[INPUT_BUFFER_SIZE];
    int inputHead;
    int inputTail;
    int64_t outputBuffer[OUTPUT_BUFFER_SIZE];
    int outputHead;
    int outputTail;
    bool halted;
} VM;

void vm_init(VM *vm);
void vm_load(VM *vm, const char *filename);
void vm_run(VM *vm);
int64_t vm_get_param_address(VM *vm, int64_t pos, int mode);
int64_t vm_get_value(VM *vm, int64_t pos, int mode);
void send_string(VM *vm, const char *s);
void reader(VM *vm);

int main() {
    VM vm;
    vm_init(&vm);
    vm_load(&vm, "input.txt");

    
    char *instructions[] = {
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "NOT A T",
        "AND A T",
        "OR E T",
        "OR H T",
        "AND T J",
        "RUN"
    };

    for (int i = 0; i < sizeof(instructions) / sizeof(instructions[0]); i++){
        send_string(&vm, instructions[i]);
    }

    vm_run(&vm);
    reader(&vm);
    

    return 0;
}

void vm_init(VM *vm) {
    memset(vm, 0, sizeof(VM));
    vm->ip = 0;
    vm->relativeBase = 0;
    vm->inputHead = 0;
    vm->inputTail = 0;
    vm->outputHead = 0;
    vm->outputTail = 0;
    vm->halted = false;
}

void vm_load(VM *vm, const char *filename) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    if ((read = getline(&line, &len, file)) != -1) {
        char *token = strtok(line, ",");
        int i = 0;
        while (token != NULL) {
           
            vm->code[i++] = atoll(token);
            token = strtok(NULL, ",");
        }
    }

    free(line);
    fclose(file);
}


void vm_run(VM *vm) {
    while (!vm->halted) {
        int64_t instruction = vm->code[vm->ip];
        int opcode = instruction % 100;
        int mode1 = (instruction / 100) % 10;
        int mode2 = (instruction / 1000) % 10;
        int mode3 = (instruction / 10000) % 10;

        int64_t param1, param2, param3;
        switch (opcode) {
            case 1:
                param1 = vm_get_value(vm, vm->ip + 1, mode1);
                param2 = vm_get_value(vm, vm->ip + 2, mode2);
                param3 = vm_get_param_address(vm, vm->ip + 3, mode3);
                vm->code[param3] = param1 + param2;
                vm->ip += 4;
                break;
            case 2:
                param1 = vm_get_value(vm, vm->ip + 1, mode1);
                param2 = vm_get_value(vm, vm->ip + 2, mode2);
                param3 = vm_get_param_address(vm, vm->ip + 3, mode3);
                vm->code[param3] = param1 * param2;
                vm->ip += 4;
                break;
            case 3:
                param1 = vm_get_param_address(vm, vm->ip + 1, mode1);
                 if (vm->inputHead == vm->inputTail) {
                   return;
                  }
                vm->code[param1] = vm->inputBuffer[vm->inputHead++];
                 if (vm->inputHead >= INPUT_BUFFER_SIZE) {
                     vm->inputHead=0;
                   }
                vm->ip += 2;
                break;
             case 4:
                 param1 = vm_get_value(vm, vm->ip + 1, mode1);
                 vm->outputBuffer[vm->outputTail++] = param1;
                 if (vm->outputTail >= OUTPUT_BUFFER_SIZE){
                     vm->outputTail = 0;
                  }
                 vm->ip += 2;
                 break;
            case 5:
                 param1 = vm_get_value(vm, vm->ip + 1, mode1);
                 param2 = vm_get_value(vm, vm->ip + 2, mode2);
                 if(param1 != 0) {
                    vm->ip = param2;
                 } else {
                    vm->ip += 3;
                 }
                break;
            case 6:
                 param1 = vm_get_value(vm, vm->ip + 1, mode1);
                 param2 = vm_get_value(vm, vm->ip + 2, mode2);
                  if(param1 == 0) {
                    vm->ip = param2;
                 } else {
                    vm->ip += 3;
                 }
                break;
           case 7:
                param1 = vm_get_value(vm, vm->ip + 1, mode1);
                param2 = vm_get_value(vm, vm->ip + 2, mode2);
                param3 = vm_get_param_address(vm, vm->ip + 3, mode3);
                vm->code[param3] = (param1 < param2) ? 1 : 0;
                vm->ip += 4;
                break;
            case 8:
                 param1 = vm_get_value(vm, vm->ip + 1, mode1);
                 param2 = vm_get_value(vm, vm->ip + 2, mode2);
                 param3 = vm_get_param_address(vm, vm->ip + 3, mode3);
                 vm->code[param3] = (param1 == param2) ? 1 : 0;
                 vm->ip += 4;
                break;
            case 9:
                param1 = vm_get_value(vm, vm->ip + 1, mode1);
                vm->relativeBase += param1;
                vm->ip += 2;
                break;
            case 99:
                vm->halted = true;
                break;
            default:
                fprintf(stderr, "Invalid opcode: %d\n", opcode);
                exit(EXIT_FAILURE);
        }
    }
}
int64_t vm_get_param_address(VM *vm, int64_t pos, int mode) {
    switch (mode) {
        case 0:
            return vm->code[pos];
        case 1:
            return pos;
        case 2:
            return vm->relativeBase + vm->code[pos];
        default:
            fprintf(stderr, "Invalid parameter mode: %d\n", mode);
            exit(EXIT_FAILURE);
    }
}


int64_t vm_get_value(VM *vm, int64_t pos, int mode) {
    int64_t address = vm_get_param_address(vm, pos, mode);
    return vm->code[address];
}

void send_string(VM *vm, const char *s) {
    for (int i = 0; s[i] != '\0'; i++) {
       
       vm->inputBuffer[vm->inputTail++] = s[i];
        if (vm->inputTail >= INPUT_BUFFER_SIZE) {
            vm->inputTail = 0;
        }
    }
    vm->inputBuffer[vm->inputTail++] = '\n';
    if (vm->inputTail >= INPUT_BUFFER_SIZE) {
            vm->inputTail = 0;
    }
}

void reader(VM *vm) {
    while (vm->outputHead != vm->outputTail) {
          int64_t c = vm->outputBuffer[vm->outputHead++];
          if (vm->outputHead >= OUTPUT_BUFFER_SIZE){
             vm->outputHead = 0;
          }
        if (c > 127) {
            printf("%lld\n", c);
            return;
        }
      
    }
}

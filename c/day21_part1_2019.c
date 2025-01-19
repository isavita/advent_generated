
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MEMORY_SIZE 4096
#define INPUT_BUFFER_SIZE 256

typedef struct {
    long long *code;
    long long ip;
    long long relativeBase;
    long long memorySize;
} VM;


void initVM(VM *vm, const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    vm->code = malloc(sizeof(long long) * MEMORY_SIZE);
    if (vm->code == NULL) {
        perror("Memory allocation error");
        fclose(fp);
        exit(EXIT_FAILURE);
    }
    vm->memorySize = MEMORY_SIZE;

    long long value;
    char comma;
    int index = 0;
    while (fscanf(fp, "%lld%c", &value, &comma) == 2) {
        vm->code[index++] = value;
        if (index >= vm->memorySize){
          vm->memorySize *= 2;
          vm->code = realloc(vm->code, sizeof(long long) * vm->memorySize);
           if (vm->code == NULL) {
                perror("Memory reallocation error");
                fclose(fp);
                exit(EXIT_FAILURE);
            }

        }

    }
    fclose(fp);
    vm->ip = 0;
    vm->relativeBase = 0;
}

void freeVM(VM *vm){
  free(vm->code);
}
long long getParamAddress(VM *vm, long long pos, int mode) {
    long long address;
    switch (mode) {
    case 0: // Position mode
      address = vm->code[pos];
        if (address < 0){
          printf("invalid memory access at: %lld\n", address);
          exit(EXIT_FAILURE);
        }

        return address;
    case 1: // Immediate mode
        return pos;
    case 2: // Relative mode
      address = vm->relativeBase + vm->code[pos];
         if (address < 0){
          printf("invalid memory access at: %lld\n", address);
          exit(EXIT_FAILURE);
        }
        return address;
    default:
        fprintf(stderr, "Invalid mode: %d\n", mode);
        exit(EXIT_FAILURE);
    }
}

void resizeMemory(VM *vm, long long address){
    if (address >= vm->memorySize){
          vm->memorySize *= 2;
          vm->code = realloc(vm->code, sizeof(long long) * vm->memorySize);
           if (vm->code == NULL) {
                perror("Memory reallocation error");
                exit(EXIT_FAILURE);
            }

        }
}
long long getParamValue(VM *vm, long long pos, int mode){
    long long address = getParamAddress(vm, pos, mode);
    resizeMemory(vm,address);
    return vm->code[address];

}

void setParamValue(VM *vm, long long pos, int mode, long long value){
    long long address = getParamAddress(vm, pos, mode);
    resizeMemory(vm,address);
    vm->code[address] = value;

}

void runVM(VM *vm) {

  char input_buffer[INPUT_BUFFER_SIZE];
  int input_index = 0;

  char *instructions[] = {
        "NOT A J\n",
        "NOT B T\n",
        "OR T J\n",
        "NOT C T\n",
        "OR T J\n",
        "AND D J\n",
        "WALK\n"
    };

  int instruction_index = 0;



    while (1) {
        long long instruction = vm->code[vm->ip];
        int opcode = instruction % 100;
        int mode1 = (instruction / 100) % 10;
        int mode2 = (instruction / 1000) % 10;
        int mode3 = (instruction / 10000) % 10;


        switch (opcode) {
            case 1: {
                long long param1 = getParamValue(vm, vm->ip + 1, mode1);
                long long param2 = getParamValue(vm, vm->ip + 2, mode2);
                setParamValue(vm, vm->ip + 3, mode3, param1 + param2);
                vm->ip += 4;
                break;
            }
            case 2: {
                long long param1 = getParamValue(vm, vm->ip + 1, mode1);
                long long param2 = getParamValue(vm, vm->ip + 2, mode2);
                setParamValue(vm, vm->ip + 3, mode3, param1 * param2);
                vm->ip += 4;
                break;
            }
           case 3: {
             if (input_index == 0) {
                if (instruction_index < 7){
                    strncpy(input_buffer, instructions[instruction_index],INPUT_BUFFER_SIZE-1);
                    input_index = 0;
                    instruction_index++;

                }
                else {
                  exit(EXIT_SUCCESS);
                }
              }
               setParamValue(vm,vm->ip+1,mode1,input_buffer[input_index]);
               input_index++;

               if(input_buffer[input_index] == 0){
                 input_index = 0;

               }
               vm->ip += 2;
              break;

            }
             case 4: {
                long long output = getParamValue(vm,vm->ip+1,mode1);
                 if (output > 127){
                    printf("%lld\n",output);
                    return;
                 }
                 printf("%c", (char)output);
                vm->ip += 2;
                break;
            }
            case 5: {
                long long param1 = getParamValue(vm, vm->ip + 1, mode1);
                long long param2 = getParamValue(vm, vm->ip + 2, mode2);
                if (param1 != 0) {
                    vm->ip = param2;
                } else {
                    vm->ip += 3;
                }
                break;
            }
            case 6: {
                long long param1 = getParamValue(vm, vm->ip + 1, mode1);
                long long param2 = getParamValue(vm, vm->ip + 2, mode2);
                if (param1 == 0) {
                    vm->ip = param2;
                } else {
                    vm->ip += 3;
                }
                break;
            }
            case 7: {
                long long param1 = getParamValue(vm, vm->ip + 1, mode1);
                long long param2 = getParamValue(vm, vm->ip + 2, mode2);
                setParamValue(vm, vm->ip + 3, mode3, (param1 < param2) ? 1 : 0);
                vm->ip += 4;
                break;
            }
            case 8: {
                long long param1 = getParamValue(vm, vm->ip + 1, mode1);
                long long param2 = getParamValue(vm, vm->ip + 2, mode2);
                setParamValue(vm, vm->ip + 3, mode3, (param1 == param2) ? 1 : 0);
                vm->ip += 4;
                break;
            }
             case 9: {
                long long param1 = getParamValue(vm, vm->ip + 1, mode1);
                vm->relativeBase += param1;
                 vm->ip += 2;
                break;
             }
            case 99:
                return;
            default:
                fprintf(stderr, "Invalid opcode: %d at position %lld\n", opcode, vm->ip);
                exit(EXIT_FAILURE);
        }
    }
}

int main() {
    VM vm;
    initVM(&vm, "input.txt");
    runVM(&vm);
    freeVM(&vm);
    return 0;
}

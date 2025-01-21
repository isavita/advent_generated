
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MEM_SIZE 10000
#define INPUT_FILE "input.txt"

typedef struct {
    long long *data;
    size_t size;
} Memory;

Memory createMemory(size_t size) {
    Memory mem;
    mem.data = (long long *)malloc(size * sizeof(long long));
    if(mem.data == NULL){
        perror("Failed to allocate memory");
        exit(EXIT_FAILURE);
    }
    mem.size = size;
    for(size_t i = 0; i < size; ++i){
        mem.data[i] = 0;
    }
    return mem;
}

void freeMemory(Memory mem) {
    free(mem.data);
}

void extendMemory(Memory *mem, size_t new_size) {
    if(new_size <= mem->size) return;
    mem->data = (long long *)realloc(mem->data, new_size * sizeof(long long));
    if(mem->data == NULL){
         perror("Failed to reallocate memory");
        exit(EXIT_FAILURE);
    }
    for(size_t i = mem->size; i < new_size; ++i){
        mem->data[i] = 0;
    }
    mem->size = new_size;
}

long long getParam(Memory mem, long long ip, long long modes, int offset, long long relativeBase) {
    int mode = 0;
    long long param = mem.data[ip + offset];
    if(offset <= 3){
      long long pow10 = 1;
      for(int i = 1; i < offset; i++)
          pow10 *= 10;
      
      mode = (modes/pow10)%10;
    }

    switch (mode) {
        case 0:
            if(param < 0){
                 fprintf(stderr,"Invalid memory access with mode 0 at pos:%lld param:%lld\n", ip, param);
                 exit(EXIT_FAILURE);
            }
           
           if(param >= mem.size){
                extendMemory(&mem, param + 1);
            }
            return mem.data[param];
        case 1:
            return param;
        case 2:
           if(relativeBase+param < 0){
                 fprintf(stderr,"Invalid memory access with mode 2 at pos:%lld param:%lld relativeBase:%lld\n", ip, param, relativeBase);
                 exit(EXIT_FAILURE);
            }

           if(relativeBase+param >= mem.size){
                extendMemory(&mem, relativeBase+param+1);
           }
            return mem.data[relativeBase + param];
        default:
           fprintf(stderr,"Unknown parameter mode: %d\n", mode);
            exit(EXIT_FAILURE);
    }
}

void setParam(Memory *mem, long long ip, long long modes, int offset, long long value, long long relativeBase) {
    int mode = 0;
    long long param = mem->data[ip + offset];
    if(offset <= 3){
      long long pow10 = 1;
      for(int i = 1; i < offset; i++)
          pow10 *= 10;
       mode = (modes/pow10)%10;
    }

    switch (mode) {
        case 0:
           if(param < 0){
                 fprintf(stderr,"Invalid memory access with mode 0 at pos:%lld param:%lld\n", ip, param);
                 exit(EXIT_FAILURE);
            }
             if(param >= mem->size){
                extendMemory(mem, param + 1);
            }
           
            mem->data[param] = value;
            break;
        case 2:
             if(relativeBase+param < 0){
                 fprintf(stderr,"Invalid memory access with mode 2 at pos:%lld param:%lld relativeBase:%lld\n", ip, param, relativeBase);
                 exit(EXIT_FAILURE);
            }

            if(relativeBase+param >= mem->size){
                extendMemory(mem, relativeBase+param + 1);
            }
            
            mem->data[relativeBase + param] = value;
            break;
        default:
             fprintf(stderr,"Unknown parameter mode: %d\n", mode);
            exit(EXIT_FAILURE);
    }
}

long long runIntcode(Memory mem) {
    long long output = 0;
    long long ip = 0;
    long long relativeBase = 0;

    while (1) {
        long long opcode = mem.data[ip] % 100;
        long long modes = mem.data[ip] / 100;

        switch (opcode) {
            case 1:
                setParam(&mem, ip, modes, 3, getParam(mem, ip, modes, 1, relativeBase) + getParam(mem, ip, modes, 2, relativeBase), relativeBase);
                ip += 4;
                break;
            case 2:
                setParam(&mem, ip, modes, 3, getParam(mem, ip, modes, 1, relativeBase) * getParam(mem, ip, modes, 2, relativeBase), relativeBase);
                ip += 4;
                break;
            case 3:
                setParam(&mem, ip, modes, 1, 1, relativeBase);
                ip += 2;
                break;
            case 4:
                output = getParam(mem, ip, modes, 1, relativeBase);
                ip += 2;
                break;
            case 5:
                if (getParam(mem, ip, modes, 1, relativeBase) != 0) {
                    ip = getParam(mem, ip, modes, 2, relativeBase);
                } else {
                    ip += 3;
                }
                break;
            case 6:
                if (getParam(mem, ip, modes, 1, relativeBase) == 0) {
                    ip = getParam(mem, ip, modes, 2, relativeBase);
                } else {
                    ip += 3;
                }
                break;
            case 7:
                if (getParam(mem, ip, modes, 1, relativeBase) < getParam(mem, ip, modes, 2, relativeBase)) {
                    setParam(&mem, ip, modes, 3, 1, relativeBase);
                } else {
                    setParam(&mem, ip, modes, 3, 0, relativeBase);
                }
                ip += 4;
                break;
            case 8:
                if (getParam(mem, ip, modes, 1, relativeBase) == getParam(mem, ip, modes, 2, relativeBase)) {
                    setParam(&mem, ip, modes, 3, 1, relativeBase);
                } else {
                    setParam(&mem, ip, modes, 3, 0, relativeBase);
                }
                ip += 4;
                break;
           case 9:
                relativeBase += getParam(mem, ip, modes, 1, relativeBase);
                ip += 2;
                break;
            case 99:
                return output;
            default:
                 fprintf(stderr,"Unknown opcode: %lld\n", opcode);
                exit(EXIT_FAILURE);
        }
    }
}

int main() {
    FILE *file = fopen(INPUT_FILE, "r");
    if (file == NULL) {
         perror("Failed to open input file");
        return EXIT_FAILURE;
    }
    Memory mem = createMemory(MEM_SIZE);
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
     if ((read = getline(&line, &len, file)) == -1) {
        perror("Failed to read line from input file");
        freeMemory(mem);
        fclose(file);
        free(line);
         return EXIT_FAILURE;
    }
    char *token = strtok(line, ",");
     long long i = 0;
    while (token != NULL) {
        mem.data[i++] = atoll(token);
        token = strtok(NULL, ",");
    }
    fclose(file);
    free(line);
    
    printf("%lld\n", runIntcode(mem));
    freeMemory(mem);
    return 0;
}

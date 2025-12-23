
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MEMORY_SIZE 4096
#define INPUT_BUFFER_SIZE 256

typedef struct {
    long long *code;
    long long ip;
    long long relativeBase;
    long long memorySize;
} VM;

void initVM(VM *vm, const char *filename) {
    NSString *path = [NSString stringWithUTF8String:filename];
    NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
    NSArray *parts = [content componentsSeparatedByString:@","];
    vm->code = malloc(sizeof(long long) * MEMORY_SIZE);
    vm->memorySize = MEMORY_SIZE;
    long long idx = 0;
    for (NSString *p in parts) {
        long long val = [p longLongValue];
        vm->code[idx++] = val;
        if (idx >= vm->memorySize) {
            vm->memorySize <<= 1;
            vm->code = realloc(vm->code, sizeof(long long) * vm->memorySize);
        }
    }
    vm->ip = 0;
    vm->relativeBase = 0;
}

void freeVM(VM *vm){ free(vm->code); }

long long getParamAddress(VM *vm, long long pos, int mode){
    long long addr;
    switch (mode){
        case 0: addr = vm->code[pos]; break;
        case 1: return pos;
        case 2: addr = vm->relativeBase + vm->code[pos]; break;
        default: return -1;
    }
    if (addr < 0) { fprintf(stderr,"invalid memory access at: %lld\n", addr); exit(EXIT_FAILURE); }
    return addr;
}

void resizeMemory(VM *vm, long long addr){
    if (addr >= vm->memorySize){
        vm->memorySize <<= 1;
        vm->code = realloc(vm->code, sizeof(long long) * vm->memorySize);
    }
}

long long getParamValue(VM *vm, long long pos, int mode){
    long long addr = getParamAddress(vm, pos, mode);
    resizeMemory(vm, addr);
    return vm->code[addr];
}

void setParamValue(VM *vm, long long pos, int mode, long long val){
    long long addr = getParamAddress(vm, pos, mode);
    resizeMemory(vm, addr);
    vm->code[addr] = val;
}

void runVM(VM *vm){
    char input_buffer[INPUT_BUFFER_SIZE];
    int input_index = 0;
    const char *instructions[] = {
        "NOT A J\n",
        "NOT B T\n",
        "OR T J\n",
        "NOT C T\n",
        "OR T J\n",
        "AND D J\n",
        "WALK\n"
    };
    int instruction_index = 0;
    while (1){
        long long inst = vm->code[vm->ip];
        int opcode = inst % 100;
        int m1 = (inst/100)%10, m2=(inst/1000)%10, m3=(inst/10000)%10;
        switch (opcode){
            case 1: {
                long long a=getParamValue(vm,vm->ip+1,m1);
                long long b=getParamValue(vm,vm->ip+2,m2);
                setParamValue(vm,vm->ip+3,m3,a+b);
                vm->ip+=4;break;}
            case 2: {
                long long a=getParamValue(vm,vm->ip+1,m1);
                long long b=getParamValue(vm,vm->ip+2,m2);
                setParamValue(vm,vm->ip+3,m3,a*b);
                vm->ip+=4;break;}
            case 3: {
                if (input_index==0 && instruction_index<7){
                    strncpy(input_buffer,instructions[instruction_index],INPUT_BUFFER_SIZE-1);
                    input_index=0;instruction_index++;
                }
                setParamValue(vm,vm->ip+1,m1,input_buffer[input_index]);
                input_index++;
                if (input_buffer[input_index]==0) input_index=0;
                vm->ip+=2;break;}
            case 4: {
                long long out=getParamValue(vm,vm->ip+1,m1);
                if (out>127){ printf("%lld\n",out); return;}
                putchar((int)out);
                vm->ip+=2;break;}
            case 5: {
                long long a=getParamValue(vm,vm->ip+1,m1);
                long long b=getParamValue(vm,vm->ip+2,m2);
                vm->ip = a?b:vm->ip+3;break;}
            case 6: {
                long long a=getParamValue(vm,vm->ip+1,m1);
                long long b=getParamValue(vm,vm->ip+2,m2);
                vm->ip = a?vm->ip+3:b;break;}
            case 7: {
                long long a=getParamValue(vm,vm->ip+1,m1);
                long long b=getParamValue(vm,vm->ip+2,m2);
                setParamValue(vm,vm->ip+3,m3,a<b?1:0);
                vm->ip+=4;break;}
            case 8: {
                long long a=getParamValue(vm,vm->ip+1,m1);
                long long b=getParamValue(vm,vm->ip+2,m2);
                setParamValue(vm,vm->ip+3,m3,a==b?1:0);
                vm->ip+=4;break;}
            case 9: {
                long long a=getParamValue(vm,vm->ip+1,m1);
                vm->relativeBase+=a;vm->ip+=2;break;}
            case 99: return;
            default:
                fprintf(stderr,"Invalid opcode %d at %lld\n",opcode,vm->ip);
                exit(EXIT_FAILURE);
        }
    }
}

int main(int argc, char *argv[]){
    VM vm;
    initVM(&vm,"input.txt");
    runVM(&vm);
    freeVM(&vm);
    return 0;
}


#import <Foundation/Foundation.h>
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

static void vm_init(VM *vm) {
    memset(vm, 0, sizeof(VM));
}
static void vm_load(VM *vm, const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) exit(EXIT_FAILURE);
    char *line = NULL;
    size_t len = 0;
    if (getline(&line, &len, f) != -1) {
        char *tok = strtok(line, ",");
        int i = 0;
        while (tok) {
            vm->code[i++] = atoll(tok);
            tok = strtok(NULL, ",");
        }
    }
    free(line);
    fclose(f);
}
static int64_t vm_get_param_address(VM *vm, int64_t pos, int mode) {
    if (mode == 0) return vm->code[pos];
    if (mode == 1) return pos;
    if (mode == 2) return vm->relativeBase + vm->code[pos];
    exit(EXIT_FAILURE);
}
static int64_t vm_get_value(VM *vm, int64_t pos, int mode) {
    return vm->code[vm_get_param_address(vm, pos, mode)];
}
static void vm_run(VM *vm) {
    while (!vm->halted) {
        int64_t instr = vm->code[vm->ip];
        int op = instr % 100;
        int m1 = (instr / 100) % 10;
        int m2 = (instr / 1000) % 10;
        int m3 = (instr / 10000) % 10;
        int64_t p1, p2, p3;
        switch (op) {
            case 1:
                p1 = vm_get_value(vm, vm->ip+1, m1);
                p2 = vm_get_value(vm, vm->ip+2, m2);
                p3 = vm_get_param_address(vm, vm->ip+3, m3);
                vm->code[p3] = p1 + p2;
                vm->ip += 4;
                break;
            case 2:
                p1 = vm_get_value(vm, vm->ip+1, m1);
                p2 = vm_get_value(vm, vm->ip+2, m2);
                p3 = vm_get_param_address(vm, vm->ip+3, m3);
                vm->code[p3] = p1 * p2;
                vm->ip += 4;
                break;
            case 3:
                p1 = vm_get_param_address(vm, vm->ip+1, m1);
                if (vm->inputHead == vm->inputTail) return;
                vm->code[p1] = vm->inputBuffer[vm->inputHead++];
                if (vm->inputHead == INPUT_BUFFER_SIZE) vm->inputHead = 0;
                vm->ip += 2;
                break;
            case 4:
                p1 = vm_get_value(vm, vm->ip+1, m1);
                vm->outputBuffer[vm->outputTail++] = p1;
                if (vm->outputTail == OUTPUT_BUFFER_SIZE) vm->outputTail = 0;
                vm->ip += 2;
                break;
            case 5:
                p1 = vm_get_value(vm, vm->ip+1, m1);
                p2 = vm_get_value(vm, vm->ip+2, m2);
                vm->ip = p1 ? p2 : vm->ip+3;
                break;
            case 6:
                p1 = vm_get_value(vm, vm->ip+1, m1);
                p2 = vm_get_value(vm, vm->ip+2, m2);
                vm->ip = p1 ? vm->ip+3 : p2;
                break;
            case 7:
                p1 = vm_get_value(vm, vm->ip+1, m1);
                p2 = vm_get_value(vm, vm->ip+2, m2);
                p3 = vm_get_param_address(vm, vm->ip+3, m3);
                vm->code[p3] = (p1 < p2);
                vm->ip += 4;
                break;
            case 8:
                p1 = vm_get_value(vm, vm->ip+1, m1);
                p2 = vm_get_value(vm, vm->ip+2, m2);
                p3 = vm_get_param_address(vm, vm->ip+3, m3);
                vm->code[p3] = (p1 == p2);
                vm->ip += 4;
                break;
            case 9:
                p1 = vm_get_value(vm, vm->ip+1, m1);
                vm->relativeBase += p1;
                vm->ip += 2;
                break;
            case 99:
                vm->halted = true;
                break;
            default:
                exit(EXIT_FAILURE);
        }
    }
}
static void send_string(VM *vm, const char *s) {
    for (int i = 0; s[i]; ++i) {
        vm->inputBuffer[vm->inputTail++] = s[i];
        if (vm->inputTail == INPUT_BUFFER_SIZE) vm->inputTail = 0;
    }
    vm->inputBuffer[vm->inputTail++] = '\n';
    if (vm->inputTail == INPUT_BUFFER_SIZE) vm->inputTail = 0;
}
static void reader(VM *vm) {
    while (vm->outputHead != vm->outputTail) {
        int64_t c = vm->outputBuffer[vm->outputHead++];
        if (vm->outputHead == OUTPUT_BUFFER_SIZE) vm->outputHead = 0;
        if (c > 127) {
            printf("%lld\n", c);
            return;
        }
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        VM vm;
        vm_init(&vm);
        vm_load(&vm, "input.txt");
        const char *inst[] = {
            "NOT A J","NOT B T","OR T J","NOT C T","OR T J","AND D J",
            "NOT A T","AND A T","OR E T","OR H T","AND T J","RUN"
        };
        for (size_t i = 0; i < sizeof(inst)/sizeof(inst[0]); ++i)
            send_string(&vm, inst[i]);
        vm_run(&vm);
        reader(&vm);
    }
    return 0;
}

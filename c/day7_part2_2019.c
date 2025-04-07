
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <limits.h>

#define NUM_AMPLIFIERS 5
#define MAX_CODE_SIZE 4096

// --- Thread-Safe Channel (Queue of size 1) ---
typedef struct {
    long long value;
    int has_value;
    pthread_mutex_t mutex;
    pthread_cond_t can_put;
    pthread_cond_t can_get;
} Channel;

void channel_init(Channel *ch) {
    ch->has_value = 0;
    pthread_mutex_init(&ch->mutex, NULL);
    pthread_cond_init(&ch->can_put, NULL);
    pthread_cond_init(&ch->can_get, NULL);
}

void channel_destroy(Channel *ch) {
    pthread_mutex_destroy(&ch->mutex);
    pthread_cond_destroy(&ch->can_put);
    pthread_cond_destroy(&ch->can_get);
}

void channel_put(Channel *ch, long long value) {
    pthread_mutex_lock(&ch->mutex);
    while (ch->has_value) {
        pthread_cond_wait(&ch->can_put, &ch->mutex);
    }
    ch->value = value;
    ch->has_value = 1;
    pthread_cond_signal(&ch->can_get);
    pthread_mutex_unlock(&ch->mutex);
}

long long channel_get(Channel *ch) {
    pthread_mutex_lock(&ch->mutex);
    while (!ch->has_value) {
        pthread_cond_wait(&ch->can_get, &ch->mutex);
    }
    long long value = ch->value;
    ch->has_value = 0;
    pthread_cond_signal(&ch->can_put);
    pthread_mutex_unlock(&ch->mutex);
    return value;
}

// --- Intcode VM ---
typedef struct {
    long long code[MAX_CODE_SIZE];
    int ip;
    Channel *input_channel;
    Channel *output_channel;
    int halted;
} VMState;

long long get_param(VMState *vm, int offset, int mode) {
    long long param_val = vm->code[vm->ip + offset];
    if (mode == 0) { // Position mode
        return vm->code[param_val];
    } else { // Immediate mode
        return param_val;
    }
}

void run_vm(VMState *vm) {
    while (vm->ip < MAX_CODE_SIZE && !vm->halted) {
        long long instruction = vm->code[vm->ip];
        int opcode = instruction % 100;
        int mode1 = (instruction / 100) % 10;
        int mode2 = (instruction / 1000) % 10;
        // Mode 3 is never immediate for writes

        long long p1, p2, addr;

        switch (opcode) {
            case 1: // Add
                p1 = get_param(vm, 1, mode1);
                p2 = get_param(vm, 2, mode2);
                addr = vm->code[vm->ip + 3];
                vm->code[addr] = p1 + p2;
                vm->ip += 4;
                break;
            case 2: // Multiply
                p1 = get_param(vm, 1, mode1);
                p2 = get_param(vm, 2, mode2);
                addr = vm->code[vm->ip + 3];
                vm->code[addr] = p1 * p2;
                vm->ip += 4;
                break;
            case 3: // Input
                addr = vm->code[vm->ip + 1];
                vm->code[addr] = channel_get(vm->input_channel);
                vm->ip += 2;
                break;
            case 4: // Output
                p1 = get_param(vm, 1, mode1);
                channel_put(vm->output_channel, p1);
                vm->ip += 2;
                break;
            case 5: // Jump-if-true
                p1 = get_param(vm, 1, mode1);
                p2 = get_param(vm, 2, mode2);
                if (p1 != 0) vm->ip = p2;
                else vm->ip += 3;
                break;
            case 6: // Jump-if-false
                p1 = get_param(vm, 1, mode1);
                p2 = get_param(vm, 2, mode2);
                if (p1 == 0) vm->ip = p2;
                else vm->ip += 3;
                break;
            case 7: // Less than
                p1 = get_param(vm, 1, mode1);
                p2 = get_param(vm, 2, mode2);
                addr = vm->code[vm->ip + 3];
                vm->code[addr] = (p1 < p2) ? 1 : 0;
                vm->ip += 4;
                break;
            case 8: // Equals
                p1 = get_param(vm, 1, mode1);
                p2 = get_param(vm, 2, mode2);
                addr = vm->code[vm->ip + 3];
                vm->code[addr] = (p1 == p2) ? 1 : 0;
                vm->ip += 4;
                break;
            case 99: // Halt
                vm->halted = 1;
                return;
            default:
                fprintf(stderr, "Unknown opcode: %d at ip %d\n", opcode, vm->ip);
                vm->halted = 1; // Stop on error
                return;
        }
    }
}

void *vm_thread_func(void *arg) {
    VMState *vm = (VMState *)arg;
    run_vm(vm);
    return NULL;
}

// --- Permutations ---
void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

int next_permutation(int *arr, int n) {
    int i = n - 2;
    while (i >= 0 && arr[i] >= arr[i + 1]) {
        i--;
    }
    if (i < 0) return 0; // Last permutation

    int j = n - 1;
    while (arr[j] <= arr[i]) {
        j--;
    }
    swap(&arr[i], &arr[j]);

    int l = i + 1, r = n - 1;
    while (l < r) {
        swap(&arr[l], &arr[r]);
        l++;
        r--;
    }
    return 1;
}

// --- Main Logic ---
long long run_feedback_loop(int *phase_settings, long long *initial_code, int code_size) {
    Channel channels[NUM_AMPLIFIERS];
    VMState vms[NUM_AMPLIFIERS];
    pthread_t threads[NUM_AMPLIFIERS];

    for (int i = 0; i < NUM_AMPLIFIERS; ++i) {
        channel_init(&channels[i]);
        memcpy(vms[i].code, initial_code, code_size * sizeof(long long));
        // Fill rest with zeros? Assuming MAX_CODE_SIZE is large enough
        memset(vms[i].code + code_size, 0, (MAX_CODE_SIZE - code_size) * sizeof(long long));
        vms[i].ip = 0;
        vms[i].halted = 0;
        vms[i].input_channel = &channels[i];
        vms[i].output_channel = &channels[(i + 1) % NUM_AMPLIFIERS];
    }

    for (int i = 0; i < NUM_AMPLIFIERS; ++i) {
        channel_put(vms[i].input_channel, phase_settings[i]);
        pthread_create(&threads[i], NULL, vm_thread_func, &vms[i]);
    }

    channel_put(vms[0].input_channel, 0); // Initial signal

    long long final_output = 0;
    // Wait for the last amplifier's output (which goes to the first channel)
    // We need to join to ensure the final value is produced correctly.
    for (int i = 0; i < NUM_AMPLIFIERS; ++i) {
        pthread_join(threads[i], NULL);
    }
    // After all threads finish, the final output signal might be waiting in the first channel.
    // However, the Python version implies the *last* read from chs[0] after threads join is the result.
    // In our channel implementation, getting from an empty channel after halt would block.
    // The final signal *must* have been placed before the last VM halted.
    // Let's retrieve the last value placed in channel 0 by VM 4.
    final_output = channels[0].value; // Read the last value put, don't use channel_get which waits

    for (int i = 0; i < NUM_AMPLIFIERS; ++i) {
        channel_destroy(&channels[i]);
    }

    return final_output;
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    long long initial_code[MAX_CODE_SIZE];
    int code_size = 0;
    while (code_size < MAX_CODE_SIZE && fscanf(file, "%lld,", &initial_code[code_size]) == 1) {
        code_size++;
    }
    fclose(file);

    if (code_size == 0) {
         fprintf(stderr, "Error reading code from input.txt\n");
         return 1;
    }


    int phase_settings[NUM_AMPLIFIERS] = {5, 6, 7, 8, 9};
    long long max_output = LLONG_MIN; // Use LLONG_MIN for safety

    do {
        long long current_output = run_feedback_loop(phase_settings, initial_code, code_size);
        if (current_output > max_output) {
            max_output = current_output;
        }
    } while (next_permutation(phase_settings, NUM_AMPLIFIERS));

    printf("%lld\n", max_output);

    return 0;
}

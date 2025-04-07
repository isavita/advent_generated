
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define INITIAL_MEM_CAPACITY 4096
#define NUM_COMPUTERS 50
#define QUEUE_CAPACITY 1024 // Should be sufficient for packets

typedef long long ll;

// --- Queue Implementation ---
typedef struct {
    ll buffer[QUEUE_CAPACITY];
    int front;
    int rear;
    int size;
} Queue;

void init_queue(Queue *q) {
    q->front = 0;
    q->rear = -1;
    q->size = 0;
}

bool is_empty_queue(Queue *q) {
    return q->size == 0;
}

bool enqueue(Queue *q, ll value) {
    if (q->size >= QUEUE_CAPACITY) {
        fprintf(stderr, "Error: Queue overflow\n");
        return false; // Indicate failure
    }
    q->rear = (q->rear + 1) % QUEUE_CAPACITY;
    q->buffer[q->rear] = value;
    q->size++;
    return true;
}

bool dequeue(Queue *q, ll *value) {
    if (is_empty_queue(q)) {
        return false; // Indicate failure
    }
    *value = q->buffer[q->front];
    q->front = (q->front + 1) % QUEUE_CAPACITY;
    q->size--;
    return true;
}

// --- Intcode Computer ---
typedef struct {
    ll *memory;
    size_t mem_capacity;
    ll ip;
    ll relative_base;
    Queue inputs;
    Queue outputs;
    bool halted;
    bool idle;
} IntcodeComputer;

void ensure_mem_capacity(IntcodeComputer *comp, size_t index) {
    if (index >= comp->mem_capacity) {
        size_t old_capacity = comp->mem_capacity;
        size_t new_capacity = old_capacity;
        while (index >= new_capacity) {
            new_capacity *= 2;
        }
        ll *new_mem = (ll *)realloc(comp->memory, new_capacity * sizeof(ll));
        if (!new_mem) {
            perror("Failed to realloc memory");
            exit(EXIT_FAILURE);
        }
        // Zero out the newly allocated memory
        memset(new_mem + old_capacity, 0, (new_capacity - old_capacity) * sizeof(ll));
        comp->memory = new_mem;
        comp->mem_capacity = new_capacity;
    }
}

ll mem_get(IntcodeComputer *comp, ll address) {
    if (address < 0) {
        fprintf(stderr, "Error: Negative memory address access: %lld\n", address);
        exit(EXIT_FAILURE);
    }
    ensure_mem_capacity(comp, (size_t)address);
    return comp->memory[address];
}

void mem_set(IntcodeComputer *comp, ll address, ll value) {
    if (address < 0) {
        fprintf(stderr, "Error: Negative memory address write: %lld\n", address);
        exit(EXIT_FAILURE);
    }
    ensure_mem_capacity(comp, (size_t)address);
    comp->memory[address] = value;
}

ll get_param_addr(IntcodeComputer *comp, int mode, ll offset) {
    ll addr = comp->ip + offset;
    ll param_val = mem_get(comp, addr);
    switch (mode) {
        case 0: // Position mode
            return param_val;
        case 2: // Relative mode
            return comp->relative_base + param_val;
        default: // Should not happen for write parameters
            fprintf(stderr, "Error: Invalid mode for set_param_addr: %d\n", mode);
            exit(EXIT_FAILURE);
    }
}

ll get_param(IntcodeComputer *comp, int mode, ll offset) {
    ll addr = comp->ip + offset;
    ll param_val = mem_get(comp, addr);
    switch (mode) {
        case 0: // Position mode
            return mem_get(comp, param_val);
        case 1: // Immediate mode
            return param_val;
        case 2: // Relative mode
            return mem_get(comp, comp->relative_base + param_val);
        default:
            fprintf(stderr, "Error: Unknown parameter mode: %d\n", mode);
            exit(EXIT_FAILURE);
    }
}

void run_computer(IntcodeComputer *comp) {
    comp->idle = false; // Assume not idle unless waiting for input

    while (true) {
        ll instruction = mem_get(comp, comp->ip);
        int opcode = instruction % 100;
        int modes[3] = {
            (int)((instruction / 100) % 10),
            (int)((instruction / 1000) % 10),
            (int)((instruction / 10000) % 10)
        };

        if (opcode == 99) {
            comp->halted = true;
            break;
        }

        ll p1, p2, addr;

        switch (opcode) {
            case 1: // Add
            case 2: // Multiply
            case 7: // Less than
            case 8: // Equals
                p1 = get_param(comp, modes[0], 1);
                p2 = get_param(comp, modes[1], 2);
                addr = get_param_addr(comp, modes[2], 3);
                ll result;
                if (opcode == 1) result = p1 + p2;
                else if (opcode == 2) result = p1 * p2;
                else if (opcode == 7) result = (p1 < p2) ? 1 : 0;
                else result = (p1 == p2) ? 1 : 0;
                mem_set(comp, addr, result);
                comp->ip += 4;
                break;
            case 3: // Input
                addr = get_param_addr(comp, modes[0], 1);
                ll input_val;
                if (!dequeue(&comp->inputs, &input_val)) {
                     // No specific input value provided externally yet for this cycle.
                     // The simulation loop must provide -1 if queue is truly empty.
                     // Here we assume the value was already enqueued.
                     // If it was -1, mark as idle.
                     fprintf(stderr, "Computer %lld requested input but queue empty in run_computer\n", (long long)(comp - (IntcodeComputer*)0)); // Hacky way to get index if array is contiguous
                     // This scenario might indicate an issue if the main loop doesn't manage input correctly.
                     // Let's assume -1 was provided by the main loop.
                     mem_set(comp, addr, -1LL);
                     comp->idle = true; // Became idle waiting for input
                     comp->ip += 2;
                     return; // Pause execution until next cycle

                }
                // Check if dequeued value is the special -1
                 if (input_val == -1LL) {
                     comp->idle = true; // Stay idle if input is -1
                 } else {
                     comp->idle = false; // Received actual input, not idle anymore
                 }
                 mem_set(comp, addr, input_val);
                 comp->ip += 2;

                 // If we just dequeued the special -1 signal, we should return
                 // to allow the network simulation to proceed. Otherwise, continue processing.
                 if (input_val == -1LL && is_empty_queue(&comp->inputs)) {
                    return;
                 }

                break;
            case 4: // Output
                p1 = get_param(comp, modes[0], 1);
                if (!enqueue(&comp->outputs, p1)) {
                     fprintf(stderr, "Output queue full!\n");
                     exit(EXIT_FAILURE); // Or handle differently
                }
                comp->ip += 2;
                comp->idle = false; // Produced output
                if (comp->outputs.size >= 3) {
                    return; // Return to let the main loop process the packet
                }
                break;
            case 5: // Jump-if-true
            case 6: // Jump-if-false
                p1 = get_param(comp, modes[0], 1);
                p2 = get_param(comp, modes[1], 2);
                if ((opcode == 5 && p1 != 0) || (opcode == 6 && p1 == 0)) {
                    comp->ip = p2;
                } else {
                    comp->ip += 3;
                }
                break;
            case 9: // Adjust relative base
                p1 = get_param(comp, modes[0], 1);
                comp->relative_base += p1;
                comp->ip += 2;
                break;
            default:
                fprintf(stderr, "Error: Unknown opcode %d at ip %lld\n", opcode, comp->ip);
                comp->halted = true; // Halt on error
                return; // Exit run loop
        }
    }
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return EXIT_FAILURE;
    }

    ll *initial_program = NULL;
    size_t program_size = 0;
    size_t program_capacity = 0;
    ll value;
    char ch;

    while (fscanf(file, "%lld", &value) == 1) {
        if (program_size >= program_capacity) {
            program_capacity = (program_capacity == 0) ? 128 : program_capacity * 2;
            ll *new_program = (ll *)realloc(initial_program, program_capacity * sizeof(ll));
            if (!new_program) {
                perror("Failed to realloc initial program memory");
                fclose(file);
                free(initial_program);
                return EXIT_FAILURE;
            }
            initial_program = new_program;
        }
        initial_program[program_size++] = value;
        // Consume the comma or newline
        if (fscanf(file, "%c", &ch) != 1 || (ch != ',' && ch != '\n' && !feof(file))) {
             if (!feof(file)){
                fprintf(stderr, "Warning: Unexpected character '%c' after number.\n", ch);
             }
        }
         if (feof(file)) break;
    }
    fclose(file);

    if (!initial_program) {
        fprintf(stderr, "Error: No program loaded from input.txt\n");
        return EXIT_FAILURE;
    }

    IntcodeComputer computers[NUM_COMPUTERS];
    Queue packet_queues[NUM_COMPUTERS];

    for (int i = 0; i < NUM_COMPUTERS; ++i) {
        computers[i].mem_capacity = (program_size > INITIAL_MEM_CAPACITY) ? program_size * 2 : INITIAL_MEM_CAPACITY;
        computers[i].memory = (ll *)malloc(computers[i].mem_capacity * sizeof(ll));
        if (!computers[i].memory) {
            perror("Failed to allocate memory for computer");
            // Need to free previously allocated resources
            for(int j = 0; j < i; ++j) free(computers[j].memory);
            free(initial_program);
            return EXIT_FAILURE;
        }
        memset(computers[i].memory, 0, computers[i].mem_capacity * sizeof(ll));
        memcpy(computers[i].memory, initial_program, program_size * sizeof(ll));

        computers[i].ip = 0;
        computers[i].relative_base = 0;
        init_queue(&computers[i].inputs);
        init_queue(&computers[i].outputs);
        computers[i].halted = false;
        computers[i].idle = false; // Initially not idle

        init_queue(&packet_queues[i]);

        // Provide initial address
        enqueue(&computers[i].inputs, (ll)i);
    }
    free(initial_program); // Initial program copied, no longer needed

    ll nat_x = -1, nat_y = -1;
    ll prev_nat_y = -2; // Initialize to a value Y cannot be
    bool nat_has_packet = false;

    while (true) {
        bool network_was_idle = true; // Assume idle unless activity detected

        for (int i = 0; i < NUM_COMPUTERS; ++i) {
            if (computers[i].halted) continue;

            // Provide input from packet queue or -1
            if (is_empty_queue(&packet_queues[i])) {
                 // Only provide -1 if the computer's internal input queue is also empty
                 if (is_empty_queue(&computers[i].inputs)) {
                    enqueue(&computers[i].inputs, -1LL);
                 }
                 // The computer might still have its address or previous packet data in its internal queue.
                 // We don't set network_was_idle = false here, as providing -1 is part of idle check.
            } else {
                network_was_idle = false; // Activity: Packet queue had data
                ll x, y;
                dequeue(&packet_queues[i], &x);
                dequeue(&packet_queues[i], &y);
                enqueue(&computers[i].inputs, x);
                enqueue(&computers[i].inputs, y);
            }

            run_computer(&computers[i]);

             // If computer did not become idle after running, network is not idle
             if (!computers[i].idle) {
                  network_was_idle = false;
             }


            // Process outputs
            while (computers[i].outputs.size >= 3) {
                network_was_idle = false; // Activity: Computer produced output
                ll dest, x, y;
                dequeue(&computers[i].outputs, &dest);
                dequeue(&computers[i].outputs, &x);
                dequeue(&computers[i].outputs, &y);

                if (dest == 255) {
                    nat_x = x;
                    nat_y = y;
                    nat_has_packet = true;
                } else if (dest >= 0 && dest < NUM_COMPUTERS) {
                    enqueue(&packet_queues[dest], x);
                    enqueue(&packet_queues[dest], y);
                } else {
                   // fprintf(stderr, "Warning: Packet sent to invalid address %lld\n", dest);
                }
            }
        }

        // Check for network idle state AFTER all computers have run for a cycle
        bool all_queues_empty_now = true;
        for (int i = 0; i < NUM_COMPUTERS; i++) {
            if (!is_empty_queue(&packet_queues[i])) {
                all_queues_empty_now = false;
                break;
            }
        }

        bool all_computers_idle_now = true;
         for (int i = 0; i < NUM_COMPUTERS; i++) {
             // A computer is considered idle for the NAT check if its packet queue is empty
             // AND its internal input queue is empty (or just contains -1) AND it signaled idle state.
             // The `computer[i].idle` flag captures the "waiting for input" state accurately.
             if (!computers[i].halted && !computers[i].idle) {
                 all_computers_idle_now = false;
                 break;
             }
         }


        if (network_was_idle && all_queues_empty_now && all_computers_idle_now) {
            if (nat_has_packet) {
                if (nat_y == prev_nat_y) {
                    printf("%lld\n", nat_y);
                    goto cleanup; // Found the answer
                }
                prev_nat_y = nat_y;
                enqueue(&packet_queues[0], nat_x);
                enqueue(&packet_queues[0], nat_y);
                computers[0].idle = false; // Wake up computer 0
                nat_has_packet = false; // NAT packet consumed
            } else {
                // Network is idle, but NAT has no packet.
                // Check if all computers have halted. If so, maybe terminate?
                bool all_halted = true;
                for(int i=0; i<NUM_COMPUTERS; ++i) {
                    if (!computers[i].halted) {
                        all_halted = false;
                        break;
                    }
                }
                if (all_halted) {
                    fprintf(stderr, "All computers halted, but no solution found.\n");
                    goto cleanup; // Exit if truly stuck
                }
                // Otherwise, just continue the loop, maybe waiting for external event (not applicable here)
            }
        }
    }

cleanup:
    for (int i = 0; i < NUM_COMPUTERS; ++i) {
        free(computers[i].memory);
        // Queues are stack-allocated, no need to free buffer if static array
    }

    return EXIT_SUCCESS;
}


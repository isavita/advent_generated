
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MEM_SIZE 8192
#define NUM_COMPUTERS 50
#define QUEUE_CAPACITY 256

typedef long long ll;

typedef struct {
    ll items[QUEUE_CAPACITY];
    int head;
    int tail;
    int count;
} Queue;

typedef struct {
    ll memory[MEM_SIZE];
    ll ip;
    ll relative_base;
    Queue input_queue;
    ll output_buffer[3];
    int output_count;
    bool halted;
    bool needs_input;
} Computer;

typedef struct {
    ll x;
    ll y;
} Packet;

typedef struct {
    Packet items[QUEUE_CAPACITY];
    int head;
    int tail;
    int count;
} PacketQueue;

void init_queue(Queue *q) {
    q->head = 0;
    q->tail = 0;
    q->count = 0;
}

bool is_queue_empty(Queue *q) {
    return q->count == 0;
}

void enqueue(Queue *q, ll value) {
    if (q->count < QUEUE_CAPACITY) {
        q->items[q->tail] = value;
        q->tail = (q->tail + 1) % QUEUE_CAPACITY;
        q->count++;
    }
}

ll dequeue(Queue *q) {
    if (q->count > 0) {
        ll value = q->items[q->head];
        q->head = (q->head + 1) % QUEUE_CAPACITY;
        q->count--;
        return value;
    }
    return -1; // Should not happen if needs_input is checked
}

void init_packet_queue(PacketQueue *q) {
    q->head = 0;
    q->tail = 0;
    q->count = 0;
}

bool is_packet_queue_empty(PacketQueue *q) {
    return q->count == 0;
}

void enqueue_packet(PacketQueue *q, Packet p) {
    if (q->count < QUEUE_CAPACITY) {
        q->items[q->tail] = p;
        q->tail = (q->tail + 1) % QUEUE_CAPACITY;
        q->count++;
    }
}

Packet dequeue_packet(PacketQueue *q) {
    Packet p = {-1, -1};
    if (q->count > 0) {
        p = q->items[q->head];
        q->head = (q->head + 1) % QUEUE_CAPACITY;
        q->count--;
    }
    return p;
}

ll get_mem(Computer *c, ll addr) {
    if (addr < 0 || addr >= MEM_SIZE) {
        fprintf(stderr, "Error: Memory access out of bounds: %lld\n", addr);
        exit(1);
    }
    return c->memory[addr];
}

void set_mem(Computer *c, ll addr, ll value) {
    if (addr < 0 || addr >= MEM_SIZE) {
        fprintf(stderr, "Error: Memory access out of bounds: %lld\n", addr);
        exit(1);
    }
    c->memory[addr] = value;
}

ll get_param(Computer *c, int mode, int offset) {
    ll val = get_mem(c, c->ip + offset);
    switch (mode) {
        case 0: // Position mode
            return get_mem(c, val);
        case 1: // Immediate mode
            return val;
        case 2: // Relative mode
            return get_mem(c, c->relative_base + val);
        default:
            fprintf(stderr, "Error: Unknown parameter mode: %d\n", mode);
            exit(1);
    }
}

void set_param(Computer *c, int mode, int offset, ll value) {
    ll addr = get_mem(c, c->ip + offset);
    switch (mode) {
        case 0: // Position mode
            set_mem(c, addr, value);
            break;
        case 2: // Relative mode
            set_mem(c, c->relative_base + addr, value);
            break;
        default:
             fprintf(stderr, "Error: Unknown parameter mode for set: %d\n", mode);
             exit(1);
    }
}

void run_computer(Computer *c) {
    c->needs_input = false;
    while (!c->halted) {
        ll instruction = get_mem(c, c->ip);
        int opcode = instruction % 100;
        int modes[3] = {
            (instruction / 100) % 10,
            (instruction / 1000) % 10,
            (instruction / 10000) % 10
        };

        ll p1, p2;

        switch (opcode) {
            case 1: // add
            case 2: // multiply
            case 7: // less than
            case 8: // equals
                p1 = get_param(c, modes[0], 1);
                p2 = get_param(c, modes[1], 2);
                ll result;
                if (opcode == 1) result = p1 + p2;
                else if (opcode == 2) result = p1 * p2;
                else if (opcode == 7) result = (p1 < p2);
                else result = (p1 == p2);
                set_param(c, modes[2], 3, result);
                c->ip += 4;
                break;
            case 3: // input
                if (is_queue_empty(&c->input_queue)) {
                    c->needs_input = true;
                    return; // Wait for input
                }
                ll input_val = dequeue(&c->input_queue);
                set_param(c, modes[0], 1, input_val);
                c->ip += 2;
                break;
            case 4: // output
                p1 = get_param(c, modes[0], 1);
                c->output_buffer[c->output_count++] = p1;
                c->ip += 2;
                if (c->output_count == 3) {
                    return; // Output ready
                }
                break;
            case 5: // jump-if-true
            case 6: // jump-if-false
                p1 = get_param(c, modes[0], 1);
                p2 = get_param(c, modes[1], 2);
                if ((opcode == 5 && p1 != 0) || (opcode == 6 && p1 == 0)) {
                    c->ip = p2;
                } else {
                    c->ip += 3;
                }
                break;
            case 9: // adjust relative base
                p1 = get_param(c, modes[0], 1);
                c->relative_base += p1;
                c->ip += 2;
                break;
            case 99: // halt
                c->halted = true;
                return;
            default:
                fprintf(stderr, "Error: Unknown opcode %d at ip %lld\n", opcode, c->ip);
                exit(1);
        }
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    ll initial_program[MEM_SIZE];
    int program_size = 0;
    while (fscanf(file, "%lld,", &initial_program[program_size]) == 1) {
        program_size++;
        if (program_size >= MEM_SIZE) {
             fprintf(stderr, "Error: Program too large for initial buffer\n");
             fclose(file);
             return 1;
        }
    }
    fclose(file);

    Computer computers[NUM_COMPUTERS];
    PacketQueue packet_queues[NUM_COMPUTERS];

    for (int i = 0; i < NUM_COMPUTERS; ++i) {
        memset(&computers[i], 0, sizeof(Computer));
        memcpy(computers[i].memory, initial_program, program_size * sizeof(ll));
        computers[i].ip = 0;
        computers[i].relative_base = 0;
        init_queue(&computers[i].input_queue);
        computers[i].output_count = 0;
        computers[i].halted = false;
        computers[i].needs_input = false;

        enqueue(&computers[i].input_queue, (ll)i); // Initial address input
        init_packet_queue(&packet_queues[i]);
    }

    for (;;) {
        for (int i = 0; i < NUM_COMPUTERS; ++i) {
            if (computers[i].needs_input && is_queue_empty(&computers[i].input_queue)) {
                 if (is_packet_queue_empty(&packet_queues[i])) {
                     enqueue(&computers[i].input_queue, -1);
                 } else {
                     Packet p = dequeue_packet(&packet_queues[i]);
                     enqueue(&computers[i].input_queue, p.x);
                     enqueue(&computers[i].input_queue, p.y);
                 }
            }

            run_computer(&computers[i]);

            if (computers[i].output_count == 3) {
                ll dest = computers[i].output_buffer[0];
                ll x_val = computers[i].output_buffer[1];
                ll y_val = computers[i].output_buffer[2];
                computers[i].output_count = 0; // Reset buffer

                if (dest == 255) {
                    printf("%lld\n", y_val);
                    return 0;
                } else if (dest >= 0 && dest < NUM_COMPUTERS) {
                    Packet p = {x_val, y_val};
                    enqueue_packet(&packet_queues[dest], p);
                }
            }
        }
    }

    return 0; // Should not be reached
}


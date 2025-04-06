
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

#define MAX_INSTRUCTIONS 100
#define MAX_LINE_LEN 50
#define QUEUE_SIZE 100000 // Increased size for safety
#define NUM_REGISTERS 26

typedef enum {
    SND, SET, ADD, MUL, MOD, RCV, JGZ
} OpCode;

typedef struct {
    bool is_reg;
    long long val; // Immediate value or register index (0-25)
} Argument;

typedef struct {
    OpCode op;
    Argument arg1;
    Argument arg2; // Not used by SND, RCV
} Instruction;

typedef struct {
    long long data[QUEUE_SIZE];
    int head;
    int tail;
    int count;
} Queue;

Instruction instructions[MAX_INSTRUCTIONS];
int instruction_count = 0;

Queue queue0, queue1;
long long registers[2][NUM_REGISTERS]; // registers[0] for prog 0, registers[1] for prog 1
int pc[2] = {0, 0}; // Program counters
bool waiting[2] = {false, false};
bool terminated[2] = {false, false};
long long send_count1 = 0;

// --- Queue Operations ---
void queue_init(Queue *q) {
    q->head = 0;
    q->tail = 0;
    q->count = 0;
}

bool queue_is_empty(Queue *q) {
    return q->count == 0;
}

bool queue_is_full(Queue *q) {
    return q->count == QUEUE_SIZE;
}

void queue_push(Queue *q, long long value) {
    if (!queue_is_full(q)) {
        q->data[q->tail] = value;
        q->tail = (q->tail + 1) % QUEUE_SIZE;
        q->count++;
    } else {
        // Handle queue full error if necessary, though unlikely with large size
        fprintf(stderr, "Error: Queue overflow\n");
        exit(EXIT_FAILURE);
    }
}

long long queue_pop(Queue *q) {
    if (!queue_is_empty(q)) {
        long long value = q->data[q->head];
        q->head = (q->head + 1) % QUEUE_SIZE;
        q->count--;
        return value;
    } else {
        // Handle queue empty error - should be checked before calling pop
        fprintf(stderr, "Error: Queue underflow\n");
        exit(EXIT_FAILURE);
    }
}
// --- End Queue Operations ---

long long get_value(Argument arg, int prog_id) {
    if (arg.is_reg) {
        // Ensure register index is valid (though parsing should handle this)
        if (arg.val >= 0 && arg.val < NUM_REGISTERS) {
             return registers[prog_id][arg.val];
        } else {
             fprintf(stderr, "Error: Invalid register index %lld\n", arg.val);
             exit(EXIT_FAILURE);
        }
    } else {
        return arg.val;
    }
}

void parse_argument(const char *s, Argument *arg) {
    if (s == NULL) {
        // Handle cases like SND/RCV which might appear to have NULL arg2
        arg->is_reg = false;
        arg->val = 0; // Default value, shouldn't be used
        return;
    }
    if (isalpha(s[0]) && s[1] == '\0') {
        arg->is_reg = true;
        arg->val = s[0] - 'a';
    } else {
        arg->is_reg = false;
        arg->val = atoll(s);
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening input.txt");
        return 1;
    }

    char line[MAX_LINE_LEN];
    char cmd_str[5];
    char arg1_str[20];
    char arg2_str[20];

    while (fgets(line, sizeof(line), file) && instruction_count < MAX_INSTRUCTIONS) {
        arg1_str[0] = '\0';
        arg2_str[0] = '\0';
        int scanned = sscanf(line, "%4s %19s %19s", cmd_str, arg1_str, arg2_str);

        if (scanned < 2) continue; // Skip invalid lines

        Instruction *instr = &instructions[instruction_count];

        if (strcmp(cmd_str, "snd") == 0) instr->op = SND;
        else if (strcmp(cmd_str, "set") == 0) instr->op = SET;
        else if (strcmp(cmd_str, "add") == 0) instr->op = ADD;
        else if (strcmp(cmd_str, "mul") == 0) instr->op = MUL;
        else if (strcmp(cmd_str, "mod") == 0) instr->op = MOD;
        else if (strcmp(cmd_str, "rcv") == 0) instr->op = RCV;
        else if (strcmp(cmd_str, "jgz") == 0) instr->op = JGZ;
        else continue; // Unknown command

        parse_argument(arg1_str, &instr->arg1);
        if (scanned == 3) {
             parse_argument(arg2_str, &instr->arg2);
        } else {
            // Ensure arg2 is marked invalid/unused for SND/RCV if needed by get_value
             instr->arg2.is_reg = false;
             instr->arg2.val = 0;
        }

        instruction_count++;
    }
    fclose(file);

    // Initialize registers and queues
    memset(registers, 0, sizeof(registers));
    registers[1]['p' - 'a'] = 1; // Program 1 starts with p=1

    queue_init(&queue0);
    queue_init(&queue1);

    while (!(terminated[0] && terminated[1]) && !(waiting[0] && waiting[1])) {
        for (int prog_id = 0; prog_id < 2; ++prog_id) {
            if (terminated[prog_id] || waiting[prog_id]) {
                 continue;
            }

            bool executed_instruction = false;
            while (pc[prog_id] >= 0 && pc[prog_id] < instruction_count) {
                Instruction instr = instructions[pc[prog_id]];
                int target_reg = instr.arg1.is_reg ? (int)instr.arg1.val : -1; // Only valid if is_reg is true

                executed_instruction = true; // Assume we execute something

                switch (instr.op) {
                    case SND:
                        if (prog_id == 0) {
                             queue_push(&queue1, get_value(instr.arg1, 0));
                        } else {
                             queue_push(&queue0, get_value(instr.arg1, 1));
                             send_count1++;
                        }
                        break;
                    case SET:
                        if(target_reg != -1) registers[prog_id][target_reg] = get_value(instr.arg2, prog_id);
                        break;
                    case ADD:
                         if(target_reg != -1) registers[prog_id][target_reg] += get_value(instr.arg2, prog_id);
                        break;
                    case MUL:
                         if(target_reg != -1) registers[prog_id][target_reg] *= get_value(instr.arg2, prog_id);
                        break;
                    case MOD:
                        if (target_reg != -1) {
                            long long val2 = get_value(instr.arg2, prog_id);
                            if (val2 != 0) {
                                registers[prog_id][target_reg] %= val2;
                            } else {
                                // Handle modulo by zero if necessary, e.g., set to 0 or error
                                registers[prog_id][target_reg] = 0;
                            }
                        }
                        break;
                    case RCV:
                        {
                            Queue *recv_queue = (prog_id == 0) ? &queue0 : &queue1;
                            if (queue_is_empty(recv_queue)) {
                                waiting[prog_id] = true;
                                executed_instruction = false; // Didn't actually execute, just waiting
                                goto next_program; // Break inner loop, signal waiting
                            } else {
                                if(target_reg != -1) registers[prog_id][target_reg] = queue_pop(recv_queue);
                                waiting[prog_id] = false; // No longer waiting if we received
                            }
                        }
                        break;
                    case JGZ:
                        if (get_value(instr.arg1, prog_id) > 0) {
                            pc[prog_id] += get_value(instr.arg2, prog_id);
                            continue; // Skip the default pc increment
                        }
                        break;
                }
                pc[prog_id]++;
                // If the other program was waiting and we just sent it a message, unwait it
                 if (instr.op == SND) {
                      int other_prog = 1 - prog_id;
                      if(waiting[other_prog]) waiting[other_prog] = false;
                 }
            } // end inner while loop for one program's execution step

            // If loop finished because pc went out of bounds
            if (!(pc[prog_id] >= 0 && pc[prog_id] < instruction_count)) {
                 terminated[prog_id] = true;
            }
            // If the program was waiting, label executed_instruction as false
            if (waiting[prog_id]) executed_instruction = false;


        next_program:; // Label for goto when RCV waits
        } // end for loop switching between programs
        // Deadlock condition check is implicit in the outer while loop condition
        // If both are waiting, the loop terminates.
         // If both terminate, the loop terminates.
         // If one terminates and the other waits indefinitely, loop terminates.
    }

    printf("%lld\n", send_count1);

    return 0;
}


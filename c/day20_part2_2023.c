
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_MODULES 64
#define MAX_NAME_LEN 16
#define MAX_CONNECTIONS 16
#define QUEUE_SIZE 16384 // Should be large enough

typedef enum {
    BROADCASTER,
    FLIP_FLOP,
    CONJUNCTION
} ModuleType;

typedef struct {
    char name[MAX_NAME_LEN];
    ModuleType type;
    int num_outputs;
    int output_indices[MAX_CONNECTIONS]; // Store indices instead of names

    // Flip-flop state
    bool ff_state;

    // Conjunction state
    int num_inputs;
    int input_indices[MAX_CONNECTIONS];  // Store indices of modules that input to this
    bool input_last_pulse[MAX_CONNECTIONS]; // Store last pulse received from each input

    // Temporary storage during parsing
    char output_names[MAX_CONNECTIONS][MAX_NAME_LEN];

} Module;

typedef struct {
    int from_module_idx;
    int to_module_idx;
    bool pulse; // false = low, true = high
} PulseState;

Module modules[MAX_MODULES];
int num_modules = 0;

PulseState queue[QUEUE_SIZE];
int queue_head = 0;
int queue_tail = 0;
int queue_count = 0;

// Helper function to find module index by name
int find_module_idx(const char* name) {
    for (int i = 0; i < num_modules; ++i) {
        if (strcmp(modules[i].name, name) == 0) {
            return i;
        }
    }
    return -1; // Not found
}

// Adds a module definition (initially stores names for outputs)
void add_module_definition(const char* line) {
    if (num_modules >= MAX_MODULES) {
        fprintf(stderr, "Error: Too many modules\n");
        exit(EXIT_FAILURE);
    }
    Module* m = &modules[num_modules];
    m->ff_state = false;
    m->num_inputs = 0;
    m->num_outputs = 0;

    char name_part[MAX_NAME_LEN * 2]; // Allow space for type prefix
    char outputs_part[256]; // Buffer for the list of outputs

    sscanf(line, "%s -> %[^\n]", name_part, outputs_part);

    char* current_name;
    if (strncmp(name_part, "broadcaster", 11) == 0) {
        m->type = BROADCASTER;
        current_name = name_part;
    } else if (name_part[0] == '%') {
        m->type = FLIP_FLOP;
        current_name = name_part + 1; // Skip '%'
    } else if (name_part[0] == '&') {
        m->type = CONJUNCTION;
        current_name = name_part + 1; // Skip '&'
    } else {
         fprintf(stderr, "Error: Unknown module type in line: %s\n", line);
         exit(EXIT_FAILURE);
    }
    strncpy(m->name, current_name, MAX_NAME_LEN - 1);
    m->name[MAX_NAME_LEN - 1] = '\0';


    // Parse outputs
    char* token = strtok(outputs_part, ", ");
    while (token != NULL && m->num_outputs < MAX_CONNECTIONS) {
        strncpy(m->output_names[m->num_outputs], token, MAX_NAME_LEN - 1);
        m->output_names[m->num_outputs][MAX_NAME_LEN - 1] = '\0';
        m->num_outputs++;
        token = strtok(NULL, ", ");
    }

    num_modules++;
}

// Resolves names to indices and sets up conjunction inputs
void resolve_connections() {
    for (int i = 0; i < num_modules; ++i) {
        Module* m = &modules[i];
        for (int j = 0; j < m->num_outputs; ++j) {
            m->output_indices[j] = find_module_idx(m->output_names[j]);
            // If output connects to a conjunction, add self as an input
            if (m->output_indices[j] != -1 && modules[m->output_indices[j]].type == CONJUNCTION) {
                Module* target_conj = &modules[m->output_indices[j]];
                if (target_conj->num_inputs < MAX_CONNECTIONS) {
                    target_conj->input_indices[target_conj->num_inputs] = i; // Store index of input module
                    target_conj->input_last_pulse[target_conj->num_inputs] = false; // Initialize memory to low
                    target_conj->num_inputs++;
                } else {
                     fprintf(stderr, "Error: Too many inputs for conjunction %s\n", target_conj->name);
                     exit(EXIT_FAILURE);
                }
            }
        }
    }
}

void enqueue(int from, int to, bool pulse) {
    if (queue_count >= QUEUE_SIZE) {
        fprintf(stderr, "Error: Queue overflow\n");
        exit(EXIT_FAILURE);
    }
    queue[queue_tail].from_module_idx = from;
    queue[queue_tail].to_module_idx = to;
    queue[queue_tail].pulse = pulse;
    queue_tail = (queue_tail + 1) % QUEUE_SIZE;
    queue_count++;
}

PulseState dequeue() {
    if (queue_count <= 0) {
        fprintf(stderr, "Error: Queue underflow\n");
        exit(EXIT_FAILURE);
    }
    PulseState state = queue[queue_head];
    queue_head = (queue_head + 1) % QUEUE_SIZE;
    queue_count--;
    return state;
}

// Greatest Common Divisor (GCD)
long long gcd(long long a, long long b) {
    while (b) {
        a %= b;
        long long temp = a;
        a = b;
        b = temp;
    }
    return a;
}

// Least Common Multiple (LCM)
long long lcm(long long a, long long b) {
    if (a == 0 || b == 0) return 0;
    return (a * b) / gcd(a, b);
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    char line[512];
    while (fgets(line, sizeof(line), fp)) {
        line[strcspn(line, "\n")] = 0; // Remove newline
        if (strlen(line) > 0) {
            add_module_definition(line);
        }
    }
    fclose(fp);

    resolve_connections();

    int broadcaster_idx = find_module_idx("broadcaster");
    if (broadcaster_idx == -1) {
         fprintf(stderr, "Error: Broadcaster module not found\n");
         return EXIT_FAILURE;
    }

    // --- Part 2 Specific Logic ---
    int rx_feeder_idx = -1;
     // Find the module that feeds into "rx" (assuming only one)
    for(int i = 0; i < num_modules; ++i) {
        for (int j = 0; j < modules[i].num_outputs; ++j) {
             // We check the *name* because "rx" might not be a parsed module itself
            if (strcmp(modules[i].output_names[j], "rx") == 0) {
                rx_feeder_idx = i;
                break;
            }
        }
        if (rx_feeder_idx != -1) break;
    }


    if (rx_feeder_idx == -1 || modules[rx_feeder_idx].type != CONJUNCTION) {
        // This problem relies on rx being fed by a single conjunction module
        // Directly printing 0 as the logic for Part 1 is removed.
        // If part 1 was needed, we'd calculate low*high pulses here.
        // For part 2, if the structure isn't as expected, we can't solve it with this method.
        printf("0\n"); // Or handle error appropriately
        fprintf(stderr, "Error: Expected a single conjunction module feeding 'rx'. Structure not supported.\n");
        return EXIT_FAILURE;
    }

    Module* feeder_conj = &modules[rx_feeder_idx];
    long long loop_lengths[MAX_CONNECTIONS];
    int num_loops_to_find = feeder_conj->num_inputs;
    int loops_found = 0;
    for(int i=0; i < num_loops_to_find; ++i) {
        loop_lengths[i] = 0; // 0 indicates not found yet
    }

    long long press_count = 0;
    while (loops_found < num_loops_to_find) {
        press_count++;
        enqueue(-1, broadcaster_idx, false); // -1 represents the button

        while (queue_count > 0) {
            PulseState current_state = dequeue();
            int from_idx = current_state.from_module_idx;
            int to_idx = current_state.to_module_idx;
            bool pulse = current_state.pulse;

             // Check if this pulse is relevant for loop detection
             // We care when an input to the rx_feeder sends a HIGH pulse
            if (to_idx == rx_feeder_idx && pulse) {
                 for(int i=0; i < feeder_conj->num_inputs; ++i) {
                     if (feeder_conj->input_indices[i] == from_idx && loop_lengths[i] == 0) {
                         loop_lengths[i] = press_count;
                         loops_found++;
                     }
                 }
            }

            if (to_idx == -1) continue; // Output connects to a non-existent module

            Module* target_module = &modules[to_idx];

            if (target_module->type == FLIP_FLOP) {
                if (!pulse) { // Only triggers on low pulse
                    target_module->ff_state = !target_module->ff_state;
                    bool pulse_to_send = target_module->ff_state;
                    for (int i = 0; i < target_module->num_outputs; ++i) {
                        enqueue(to_idx, target_module->output_indices[i], pulse_to_send);
                    }
                }
            } else if (target_module->type == CONJUNCTION) {
                // Update the memory for the input that sent the pulse
                bool all_high = true;
                for (int i = 0; i < target_module->num_inputs; ++i) {
                    if (target_module->input_indices[i] == from_idx) {
                        target_module->input_last_pulse[i] = pulse;
                    }
                    if (!target_module->input_last_pulse[i]) {
                        all_high = false;
                    }
                }
                bool pulse_to_send = !all_high;
                 for (int i = 0; i < target_module->num_outputs; ++i) {
                    enqueue(to_idx, target_module->output_indices[i], pulse_to_send);
                }
            } else { // BROADCASTER (or button initial pulse)
                 bool pulse_to_send = pulse;
                 for (int i = 0; i < target_module->num_outputs; ++i) {
                    enqueue(to_idx, target_module->output_indices[i], pulse_to_send);
                }
            }
        }
        if (loops_found == num_loops_to_find) {
             break;
        }
         // Safety break for potentially infinite loops if input is weird
         if (press_count > 1000000) {
              fprintf(stderr, "Warning: Exceeded 1,000,000 presses, may be infinite loop or very long cycle.\n");
              break;
         }
    }

    long long final_lcm = 1;
    if (loops_found == num_loops_to_find) {
        final_lcm = loop_lengths[0];
         for (int i = 1; i < num_loops_to_find; ++i) {
             final_lcm = lcm(final_lcm, loop_lengths[i]);
         }
    } else {
        fprintf(stderr, "Error: Could not find all required loop lengths.\n");
        final_lcm = 0; // Indicate failure
    }


    printf("%lld\n", final_lcm);

    return EXIT_SUCCESS;
}

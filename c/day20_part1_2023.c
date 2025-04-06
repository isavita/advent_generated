
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LEN 256
#define MAX_NAME_LEN 20
#define MAX_DESTINATIONS 10
#define MAX_MODULES 64 // Adjusted based on typical input sizes
#define HASH_TABLE_SIZE 128 // Power of 2, > MAX_MODULES
#define QUEUE_SIZE 2048
#define NUM_CYCLES 1000

typedef enum {
    LOW = 0,
    HIGH = 1
} PulseValue;

typedef enum {
    FLIPFLOP = '%',
    CONJUNCTION = '&',
    BROADCASTER = 'b',
    OTHER // For untyped modules like output
} ModuleType;

// Forward declare Module
typedef struct Module Module;

// For Conjunction memory - store index of input module and its last pulse
typedef struct {
    int input_module_idx;
    PulseValue last_pulse;
} MemoryEntry;

struct Module {
    char name[MAX_NAME_LEN];
    ModuleType type;
    int destination_indices[MAX_DESTINATIONS];
    int num_destinations;
    int id; // Index in the modules array

    // Flip-flop state
    bool state; // false = off, true = on

    // Conjunction memory
    MemoryEntry memory[MAX_DESTINATIONS]; // Max inputs <= max destinations feeding it
    int num_memory_inputs;
};

typedef struct {
    PulseValue value;
    int from_module_idx;
    int to_module_idx;
} Pulse;

// Global module storage and lookup
Module modules[MAX_MODULES];
int module_count = 0;
int module_indices[HASH_TABLE_SIZE]; // Simple hash table: name -> index in modules array

// Pulse Queue (Circular Buffer)
Pulse pulse_queue[QUEUE_SIZE];
int queue_head = 0;
int queue_tail = 0;
int queue_count = 0; // Number of items currently in the queue

// --- Hash Table Functions ---
unsigned long hash(const char *str) {
    unsigned long hash_val = 5381;
    int c;
    while ((c = *str++))
        hash_val = ((hash_val << 5) + hash_val) + c; /* hash * 33 + c */
    return hash_val % HASH_TABLE_SIZE;
}

// Very simple hash table assuming no collisions for typical AoC inputs
// If collisions happen, this needs chaining or open addressing.
void add_module_to_map(const char *name, int index) {
    unsigned long h = hash(name);
    // Simple collision handling - linear probing (or just overwrite for this simple case if names are unique)
    while(module_indices[h] != -1) {
        h = (h + 1) % HASH_TABLE_SIZE; // Very basic linear probe
    }
    module_indices[h] = index;
}

int find_module_index(const char *name) {
    unsigned long h = hash(name);
    // Probe to find
     while(module_indices[h] != -1) {
        if (strcmp(modules[module_indices[h]].name, name) == 0) {
             return module_indices[h];
        }
        h = (h + 1) % HASH_TABLE_SIZE;
    }
    return -1; // Not found
}


// --- Queue Functions ---
void enqueue(Pulse p) {
    if (queue_count >= QUEUE_SIZE) {
        fprintf(stderr, "Error: Pulse queue overflow\n");
        exit(EXIT_FAILURE);
    }
    pulse_queue[queue_tail] = p;
    queue_tail = (queue_tail + 1) % QUEUE_SIZE;
    queue_count++;
}

Pulse dequeue() {
    if (queue_count <= 0) {
        fprintf(stderr, "Error: Pulse queue underflow\n");
        // Return a dummy/error pulse? Or exit? Let's return a dummy for now.
        Pulse dummy = {LOW, -1, -1};
        return dummy;
        // exit(EXIT_FAILURE);
    }
    Pulse p = pulse_queue[queue_head];
    queue_head = (queue_head + 1) % QUEUE_SIZE;
    queue_count--;
    return p;
}

// --- Parsing ---
void parse_input(FILE *fp) {
    char line[MAX_LINE_LEN];
    char name_buf[MAX_NAME_LEN];
    char dest_buf[MAX_LINE_LEN]; // Buffer for all destinations string
    char *token;

    // Initialize hash table indices to -1 (empty)
    for(int i = 0; i < HASH_TABLE_SIZE; ++i) {
        module_indices[i] = -1;
    }

    // First pass: Create modules and store names
    while (fgets(line, sizeof(line), fp) != NULL) {
        if (module_count >= MAX_MODULES) {
             fprintf(stderr, "Error: Too many modules defined\n");
             exit(EXIT_FAILURE);
        }

        line[strcspn(line, "\n")] = 0; // Remove newline

        Module *m = &modules[module_count];
        m->id = module_count;
        m->num_destinations = 0;
        m->num_memory_inputs = 0;
        m->state = false; // Default off for flip-flops

        char *arrow_pos = strstr(line, " -> ");
        if (!arrow_pos) continue; // Skip invalid lines

        *arrow_pos = '\0'; // Split line at " -> "
        char *name_part = line;
        char *dest_part = arrow_pos + 4;

        // Determine type and name
        if (name_part[0] == FLIPFLOP || name_part[0] == CONJUNCTION) {
            m->type = (ModuleType)name_part[0];
            strncpy(m->name, name_part + 1, MAX_NAME_LEN - 1);
            m->name[MAX_NAME_LEN - 1] = '\0';
        } else if (strcmp(name_part, "broadcaster") == 0) {
            m->type = BROADCASTER;
            strncpy(m->name, name_part, MAX_NAME_LEN - 1);
             m->name[MAX_NAME_LEN - 1] = '\0';
        } else {
            // Should not happen based on problem description, but handle defensively
             m->type = OTHER;
             strncpy(m->name, name_part, MAX_NAME_LEN - 1);
             m->name[MAX_NAME_LEN - 1] = '\0';
        }

        add_module_to_map(m->name, module_count);

        // Store destination names temporarily (will resolve to indices in second pass)
        strncpy(dest_buf, dest_part, sizeof(dest_buf) - 1);
        dest_buf[sizeof(dest_buf) - 1] = '\0';
        // Store dest_buf pointer or copy? Let's store the full string temporarily.
        // We'll use a temporary storage for destination names linked to module index
        // Or, parse destinations now but resolve indices later.

        module_count++;
    }

    // Second pass: Resolve destination indices and initialize conjunction memory
    rewind(fp); // Go back to the beginning of the file
    int current_module_idx = 0;
     while (fgets(line, sizeof(line), fp) != NULL && current_module_idx < module_count) {
        line[strcspn(line, "\n")] = 0;
        char *arrow_pos = strstr(line, " -> ");
        if (!arrow_pos) continue;
        char *dest_part = arrow_pos + 4;

        Module *m = &modules[current_module_idx];

        // Parse destinations again and find their indices
        token = strtok(dest_part, ", ");
        while (token != NULL) {
            if (m->num_destinations >= MAX_DESTINATIONS) {
                 fprintf(stderr, "Error: Too many destinations for module %s\n", m->name);
                 exit(EXIT_FAILURE);
            }
            int dest_idx = find_module_index(token);
            // We might point to modules not defined (like 'output'), store -1
            m->destination_indices[m->num_destinations++] = dest_idx;

            token = strtok(NULL, ", ");
        }
         current_module_idx++;
    }

     // Third pass: Initialize conjunction memory
    for (int i = 0; i < module_count; ++i) {
        Module *source_mod = &modules[i];
        for (int j = 0; j < source_mod->num_destinations; ++j) {
            int dest_idx = source_mod->destination_indices[j];
            if (dest_idx != -1 && modules[dest_idx].type == CONJUNCTION) {
                Module *dest_mod = &modules[dest_idx];
                 if (dest_mod->num_memory_inputs >= MAX_DESTINATIONS) {
                     fprintf(stderr, "Error: Too many inputs for conjunction module %s\n", dest_mod->name);
                     exit(EXIT_FAILURE);
                 }
                dest_mod->memory[dest_mod->num_memory_inputs].input_module_idx = i; // Store source index
                dest_mod->memory[dest_mod->num_memory_inputs].last_pulse = LOW;
                dest_mod->num_memory_inputs++;
            }
        }
    }
}


// --- Simulation ---
unsigned long long push_button() {
    unsigned long long low_pulse_count = 0;
    unsigned long long high_pulse_count = 0;
    int broadcaster_idx = find_module_index("broadcaster");

    if (broadcaster_idx == -1) {
        fprintf(stderr, "Error: 'broadcaster' module not found.\n");
        exit(EXIT_FAILURE);
    }

    for (int cycle = 0; cycle < NUM_CYCLES; ++cycle) {
        // Button press sends a LOW pulse to the broadcaster
        Pulse start_pulse = {LOW, -1 /* from button */, broadcaster_idx};
        enqueue(start_pulse);

        while (queue_count > 0) {
            Pulse current_pulse = dequeue();

            // Count the pulse
            if (current_pulse.value == LOW) {
                low_pulse_count++;
            } else {
                high_pulse_count++;
            }

            // If the destination module doesn't exist (e.g., output), skip processing
            if (current_pulse.to_module_idx == -1) {
                continue;
            }

            Module *dest_mod = &modules[current_pulse.to_module_idx];
            PulseValue pulse_to_send = LOW; // Default value, might be overwritten
            bool send_pulse = true; // Flag to control if pulse should be sent

            switch (dest_mod->type) {
                case FLIPFLOP:
                    if (current_pulse.value == LOW) {
                        dest_mod->state = !dest_mod->state; // Flip state
                        pulse_to_send = dest_mod->state ? HIGH : LOW;
                    } else {
                        send_pulse = false; // High pulse ignored by flip-flops
                    }
                    break;

                case CONJUNCTION:
                    // Update memory for this input
                    for (int i = 0; i < dest_mod->num_memory_inputs; ++i) {
                        if (dest_mod->memory[i].input_module_idx == current_pulse.from_module_idx) {
                            dest_mod->memory[i].last_pulse = current_pulse.value;
                            break;
                        }
                    }
                    // Check if all inputs are HIGH
                    bool all_high = true;
                    for (int i = 0; i < dest_mod->num_memory_inputs; ++i) {
                        if (dest_mod->memory[i].last_pulse == LOW) {
                            all_high = false;
                            break;
                        }
                    }
                    pulse_to_send = all_high ? LOW : HIGH;
                    break;

                case BROADCASTER:
                    pulse_to_send = current_pulse.value;
                    break;

                case OTHER: // Modules like 'output' might exist but don't process/send further
                     send_pulse = false;
                     break;
            }

            // If a pulse needs to be sent, enqueue pulses for all destinations
            if (send_pulse) {
                for (int i = 0; i < dest_mod->num_destinations; ++i) {
                    Pulse next_pulse = {
                        pulse_to_send,
                        dest_mod->id, // from this module's index
                        dest_mod->destination_indices[i] // to destination index
                    };
                    enqueue(next_pulse);
                }
            }
        }
    }

    return low_pulse_count * high_pulse_count;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening input file");
        return 1;
    }

    parse_input(fp);
    fclose(fp);

    unsigned long long result = push_button();

    printf("%llu\n", result);

    return 0;
}

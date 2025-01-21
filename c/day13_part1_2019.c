
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef enum {
    position,
    immediate,
    relative
} mode;

typedef enum {
    add = 1,
    mul,
    input,
    output,
    jt,
    jf,
    lt,
    eq,
    rbo,
    halt = 99
} opcode;

typedef struct {
    int* data;
    size_t data_size;
    size_t ip;
    int relbase;
} Machine;

void resize_data(Machine* m, size_t new_size) {
    if (new_size <= m->data_size) return;
    int* new_data = realloc(m->data, new_size * sizeof(int));
    if (!new_data) {
        perror("Failed to reallocate memory for data");
        exit(EXIT_FAILURE);
    }
    for(size_t i = m->data_size; i < new_size; i++){
        new_data[i] = 0;
    }
    m->data = new_data;
    m->data_size = new_size;
}

Machine* create_machine(int* program, size_t program_size) {
    Machine* m = malloc(sizeof(Machine));
    if (!m) {
        perror("Failed to allocate memory for machine");
        exit(EXIT_FAILURE);
    }
    m->data_size = program_size * 2;
    m->data = malloc(m->data_size * sizeof(int));
    if (!m->data) {
       perror("Failed to allocate memory for data");
        exit(EXIT_FAILURE);
    }
    memcpy(m->data, program, program_size * sizeof(int));
    for(size_t i = program_size; i < m->data_size; i++){
        m->data[i] = 0;
    }
    m->ip = 0;
    m->relbase = 0;
    return m;
}

void free_machine(Machine* m) {
    free(m->data);
    free(m);
}

void decode(int n, opcode* op, mode* modes) {
    *op = n % 100;
    n /= 100;
    for (int i = 0; i < 3; i++) {
        modes[i] = n % 10;
        n /= 10;
    }
}

int get_value(Machine* m, size_t i, mode mo) {
    size_t addr;
    switch (mo) {
    case immediate:
        return m->data[i];
    case position:
        addr = m->data[i];
        if (addr >= m->data_size) resize_data(m, addr + 1);
        return m->data[addr];
    case relative:
        addr = m->relbase + m->data[i];
        if (addr >= m->data_size) resize_data(m, addr + 1);
        return m->data[addr];
    default:
        fprintf(stderr, "Unknown mode: %d\n", mo);
        exit(EXIT_FAILURE);
    }
    return 0;
}

void set_value(Machine* m, size_t i, mode mo, int val) {
     size_t addr;
    switch (mo) {
    case position:
        addr = m->data[i];
        if (addr >= m->data_size) resize_data(m, addr + 1);
        m->data[addr] = val;
        break;
    case relative:
         addr = m->relbase + m->data[i];
        if (addr >= m->data_size) resize_data(m, addr + 1);
        m->data[addr] = val;
        break;
    default:
        fprintf(stderr, "Unknown mode: %d\n", mo);
        exit(EXIT_FAILURE);
    }
}


bool step(Machine* m, int* output_val) {
    opcode op;
    mode modes[3];
    decode(m->data[m->ip], &op, modes);

    switch (op) {
        case add: {
            int val = get_value(m, m->ip + 1, modes[0]) + get_value(m, m->ip + 2, modes[1]);
            set_value(m, m->ip + 3, modes[2], val);
            m->ip += 4;
            break;
        }
        case mul: {
            int val = get_value(m, m->ip + 1, modes[0]) * get_value(m, m->ip + 2, modes[1]);
            set_value(m, m->ip + 3, modes[2], val);
            m->ip += 4;
            break;
        }
        case input: {
            set_value(m, m->ip + 1, modes[0], 0);
            m->ip += 2;
            break;
        }
         case output: {
            *output_val = get_value(m, m->ip + 1, modes[0]);
            m->ip += 2;
            return true;
        }
        case jt: {
             if (get_value(m, m->ip + 1, modes[0]) != 0) {
                 m->ip = get_value(m, m->ip + 2, modes[1]);
             } else {
                 m->ip += 3;
             }
            break;
        }
         case jf: {
            if (get_value(m, m->ip + 1, modes[0]) == 0) {
                m->ip = get_value(m, m->ip + 2, modes[1]);
            } else {
                m->ip += 3;
            }
            break;
        }
        case lt: {
            int val = get_value(m, m->ip + 1, modes[0]) < get_value(m, m->ip + 2, modes[1]);
            set_value(m, m->ip + 3, modes[2], val);
            m->ip += 4;
            break;
        }
        case eq: {
             int val = get_value(m, m->ip + 1, modes[0]) == get_value(m, m->ip + 2, modes[1]);
            set_value(m, m->ip + 3, modes[2], val);
            m->ip += 4;
            break;
        }
        case rbo: {
            m->relbase += get_value(m, m->ip + 1, modes[0]);
            m->ip += 2;
            break;
        }
        case halt:
            return false;
        default:
            fprintf(stderr, "Unknown opcode: %d\n", op);
            exit(EXIT_FAILURE);
    }
    return true;
}


int count_blocks(int* program, size_t program_size) {
    Machine* m = create_machine(program, program_size);
    int x, y, tile_id, block_count = 0;
    int output_count = 0;

    while (true) {
      int output_val;
      bool has_output = step(m, &output_val);
      if (!has_output) break;
          
      if(output_count % 3 == 0){
            x = output_val;
        } else if (output_count % 3 == 1){
            y = output_val;
        } else {
            tile_id = output_val;
            if (tile_id == 2) {
                block_count++;
            }
        }
         output_count++;
    }

    free_machine(m);
    return block_count;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        perror("Failed to open input file");
        return EXIT_FAILURE;
    }

    int* program = NULL;
    size_t program_size = 0;
    char* line = NULL;
    size_t line_size = 0;

    if(getline(&line, &line_size, file) != -1){
         char* token = strtok(line, ",");
        while(token){
             program = realloc(program, (program_size + 1) * sizeof(int));
             if(!program){
                 perror("Failed to allocate memory for program");
                 exit(EXIT_FAILURE);
             }
             program[program_size++] = atoi(token);
             token = strtok(NULL, ",");
        }
    }
    free(line);
    fclose(file);
    
    printf("%d\n", count_blocks(program, program_size));
    free(program);
    return EXIT_SUCCESS;
}

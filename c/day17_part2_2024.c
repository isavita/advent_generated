
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct {
    int64_t a;
    int64_t b;
    int64_t c;
    int* program;
    int program_len;
} Program;

int64_t computeOperand(int val, int64_t a, int64_t b, int64_t c) {
    switch (val) {
        case 0:
        case 1:
        case 2:
        case 3:
            return val;
        case 4:
            return a;
        case 5:
            return b;
        case 6:
            return c;
        default:
            fprintf(stderr, "Invalid combo operand: %d\n", val);
            exit(1);
    }
}

int* simulateComputer(Program program, int* out_len) {
    int* outs = NULL;
    int outs_capacity = 0;
    *out_len = 0;
    int64_t a = program.a;
    int64_t b = program.b;
    int64_t c = program.c;
    int* input = program.program;
    int input_len = program.program_len;
    
    for (int i = 0; i < input_len; i += 2) {
        int cmd = input[i];
        switch (cmd) {
            case 0:
                a >>= computeOperand(input[i+1], a, b, c);
                break;
            case 1:
                b ^= input[i+1];
                break;
            case 2:
                 b = computeOperand(input[i+1], a, b, c) % 8;
                break;
            case 3:
                if (a != 0) {
                     i = input[i+1] - 2;
                }
                break;
            case 4:
                b ^= c;
                break;
            case 5:
                if(*out_len == outs_capacity) {
                   outs_capacity = (outs_capacity ==0) ? 1 : outs_capacity *2;
                    outs = (int*) realloc(outs, outs_capacity * sizeof(int));
                }
                 outs[*out_len] = computeOperand(input[i+1], a, b, c) % 8;
                (*out_len)++;
                break;
            case 6:
                b = a >> computeOperand(input[i+1], a, b, c);
                break;
             case 7:
                c = a >> computeOperand(input[i+1], a, b, c);
                break;
            default:
                 fprintf(stderr, "Invalid opcode: %d\n", cmd);
                 exit(1);
        }
    }
    
    return outs;
}


typedef struct {
    int a;
    int64_t b;
} Pair;


int comparePairs(const void* a, const void* b) {
  const Pair* pairA = (const Pair*)a;
  const Pair* pairB = (const Pair*)b;

  if (pairA->a != pairB->a) {
      return pairA->a - pairB->a;
  }
  if (pairA->b < pairB->b){
    return -1;
  }
  if (pairA->b > pairB->b){
    return 1;
  }
  return 0;
}

int64_t* check(Program p, int* valid_len) {
  int64_t* valids = NULL;
  int valids_capacity = 0;
  *valid_len = 0;

  Pair *stack = NULL;
  int stack_size = 0;
  int stack_capacity = 0;

    Pair initial = {0, 0};
    if(stack_capacity == stack_size){
        stack_capacity = (stack_capacity == 0)? 1: stack_capacity *2;
        stack = (Pair*) realloc(stack, stack_capacity * sizeof(Pair));
    }

    stack[stack_size++] = initial;

    Pair* seen = NULL;
    int seen_size = 0;
    int seen_capacity = 0;


    while (stack_size > 0) {
        Pair state = stack[--stack_size];

        bool found = false;
        for (int i = 0; i < seen_size; ++i){
            if(seen[i].a == state.a && seen[i].b == state.b){
                found = true;
                break;
            }
        }
        if(found) continue;
      
        if(seen_size == seen_capacity){
          seen_capacity = (seen_capacity == 0) ? 1 : seen_capacity * 2;
          seen = (Pair*) realloc(seen, seen_capacity * sizeof(Pair));
        }

        seen[seen_size++] = state;


        int depth = state.a;
        int64_t score = state.b;
        
        if (depth == p.program_len) {
          if (*valid_len == valids_capacity){
            valids_capacity = (valids_capacity == 0) ? 1: valids_capacity * 2;
             valids = (int64_t*) realloc(valids, valids_capacity * sizeof(int64_t));
          }
          valids[*valid_len] = score;
          (*valid_len)++;
        } else {
             for (int64_t i = 0; i < 8; i++) {
                int64_t newScore = i + 8*score;
                Program testProgram = {newScore, p.b, p.c, p.program, p.program_len};
                int out_len;
                int* result = simulateComputer(testProgram, &out_len);
                if (out_len > 0 && result[0] == p.program[p.program_len-1-depth]) {
                  if (stack_size == stack_capacity){
                    stack_capacity = (stack_capacity == 0)? 1: stack_capacity *2;
                     stack = (Pair*) realloc(stack, stack_capacity * sizeof(Pair));
                  }
                    stack[stack_size].a = depth + 1;
                    stack[stack_size].b = newScore;
                    stack_size++;

                }
                free(result);
            }
        }
    }
    free(stack);
    free(seen);
    return valids;
}

int main() {
  FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
      perror("Error opening file");
      return 1;
    }

    int64_t a, b, c;
    int* program = NULL;
    int program_len = 0;
    int program_capacity = 0;

    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    while ((read = getline(&line, &len, file)) != -1) {
        char *prefix = strtok(line, ":");
        char *value = strtok(NULL, "\n");
      
        if (value == NULL) continue;

        if (strcmp(prefix, "Register A") == 0) {
            a = atoll(value);
        } else if (strcmp(prefix, "Register B") == 0) {
            b = atoll(value);
        } else if (strcmp(prefix, "Register C") == 0) {
            c = atoll(value);
        } else if (strcmp(prefix, "Program") == 0) {
             char* num_str = strtok(value, ",");
            while (num_str != NULL) {
                 if(program_len == program_capacity) {
                     program_capacity = (program_capacity == 0) ? 1 : program_capacity * 2;
                    program = (int*) realloc(program, program_capacity * sizeof(int));
                }
                program[program_len++] = atoi(num_str);
                num_str = strtok(NULL, ",");
            }
         }
    }
    fclose(file);
    if (line)
        free(line);

    Program p = {a, b, c, program, program_len};

    int valid_len;
    int64_t* validValues = check(p, &valid_len);
    if(valid_len == 0){
      printf("No valid values found\n");
      free(program);
      return 0;
    }

    int64_t minVal = validValues[0];
    for (int i = 1; i < valid_len; i++) {
      if (validValues[i] < minVal) {
        minVal = validValues[i];
      }
    }
    
    printf("%lld\n", minVal);
    free(validValues);
    free(program);
    return 0;
}

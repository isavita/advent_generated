
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define NUM_REGISTERS 4
#define MAX_INSTRUCTIONS 1000

typedef struct {
    char instruction[4];
    char arg1[10];
    char arg2[10];
} Instruction;

int is_number(const char *str) {
    if (str == NULL || *str == '\0') {
        return 0;
    }
    if (*str == '-') {
        str++;
    }
    while (*str) {
        if (!isdigit(*str)) {
            return 0;
        }
        str++;
    }
    return 1;
}

int get_value(const char *arg, int registers[]) {
    if (is_number(arg)) {
        return atoi(arg);
    } else if (strlen(arg) == 1 && isalpha(arg[0])) {
          return registers[arg[0] - 'a'];
    }
     return 0;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    Instruction instructions[MAX_INSTRUCTIONS];
    int num_instructions = 0;
    char line[100];

    while (fgets(line, sizeof(line), fp) != NULL) {
        sscanf(line, "%s %s %s", instructions[num_instructions].instruction, instructions[num_instructions].arg1, instructions[num_instructions].arg2);
        num_instructions++;
    }

    fclose(fp);

    
    for (int part = 1; part <= 2; part++) {
         int registers[NUM_REGISTERS] = {0};
            if (part == 2){
              registers[2] = 1; 
            }
        int pc = 0;

        while (pc >= 0 && pc < num_instructions) {
            Instruction current = instructions[pc];
             if (strcmp(current.instruction, "cpy") == 0) {
                  int val = get_value(current.arg1,registers);
                 
                if (strlen(current.arg2) == 1 && isalpha(current.arg2[0])){
                   registers[current.arg2[0] - 'a'] = val;
                }
                pc++;
             }
             else if (strcmp(current.instruction, "inc") == 0){
                  if (strlen(current.arg1) == 1 && isalpha(current.arg1[0])){
                      registers[current.arg1[0] - 'a']++;
                  }
                pc++;
              }
             else if (strcmp(current.instruction, "dec") == 0){
                if (strlen(current.arg1) == 1 && isalpha(current.arg1[0])){
                    registers[current.arg1[0] - 'a']--;
                  }
                pc++;
              }
             else if (strcmp(current.instruction, "jnz") == 0){
                    int val = get_value(current.arg1,registers);
                   if(val != 0){
                        pc += get_value(current.arg2, registers);
                   }else {
                       pc++;
                   }
            }
            else {
                pc++;
            }
        }
        printf("Part %d: Register a = %d\n", part, registers[0]);
    }
   
    return 0;
}

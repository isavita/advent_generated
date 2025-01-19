
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_INSTRUCTIONS 1000
#define MAX_LINE_LENGTH 50

typedef struct {
    char op[4];
    int arg1_type; // 0: register, 1: immediate
    int arg1_val;
    int arg2_type;
    int arg2_val;
} Instruction;

int is_reg(char *s) {
    return (s[0] >= 'a' && s[0] <= 'd' && s[1] == '\0');
}

int reg_to_idx(char reg) {
    return reg - 'a';
}

int parse_value(char *s, int *registers) {
    if (is_reg(s)) {
        return registers[reg_to_idx(s[0])];
    } else {
        return atoi(s);
    }
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    Instruction instructions[MAX_INSTRUCTIONS];
    int num_instructions = 0;
    char line[MAX_LINE_LENGTH];

    while (fgets(line, sizeof(line), fp) != NULL) {
        Instruction *instr = &instructions[num_instructions++];
        char *token = strtok(line, " \n");
        strcpy(instr->op, token);

        token = strtok(NULL, " \n");
        if (token != NULL) {
            if(is_reg(token)){
              instr->arg1_type = 0;
              instr->arg1_val = reg_to_idx(token[0]);
            } else {
              instr->arg1_type = 1;
              instr->arg1_val = atoi(token);
            }
            
            token = strtok(NULL, " \n");
             if (token != NULL) {
                if(is_reg(token)){
                    instr->arg2_type = 0;
                    instr->arg2_val = reg_to_idx(token[0]);
                } else {
                     instr->arg2_type = 1;
                     instr->arg2_val = atoi(token);
                }
            }
        }
    }

    fclose(fp);

    int registers[4] = {12, 0, 0, 0};
    int i = 0;

    while (i < num_instructions) {
        Instruction *instr = &instructions[i];
        int val1, val2;
         if(instr->arg1_type == 0) {
            val1 = registers[instr->arg1_val];
         } else {
            val1 = instr->arg1_val;
         }
        if (strcmp(instr->op, "cpy") == 0) {
           registers[instr->arg2_val] = val1;
        } else if (strcmp(instr->op, "inc") == 0) {
            registers[instr->arg1_val]++;
        } else if (strcmp(instr->op, "dec") == 0) {
             registers[instr->arg1_val]--;
        } else if (strcmp(instr->op, "jnz") == 0) {
            if (val1 != 0) {
               if(instr->arg2_type == 0) {
                val2 = registers[instr->arg2_val];
               } else {
                  val2 = instr->arg2_val;
               }
                i += val2;
                continue;
            }
        } else if (strcmp(instr->op, "tgl") == 0) {
            int target_index = i + val1;
            if (target_index >= 0 && target_index < num_instructions) {
                Instruction *target_instr = &instructions[target_index];
                if (strcmp(target_instr->op, "inc") == 0) {
                   strcpy(target_instr->op, "dec");
                } else if (strcmp(target_instr->op, "dec") == 0 || strcmp(target_instr->op, "tgl") == 0) {
                    strcpy(target_instr->op, "inc");
                } else if (strcmp(target_instr->op, "jnz") == 0) {
                     strcpy(target_instr->op, "cpy");
                }else if (strcmp(target_instr->op, "cpy") == 0) {
                     strcpy(target_instr->op, "jnz");
                }
            }
        }
        i++;
    }

    printf("%d\n", registers[0]);

    return 0;
}

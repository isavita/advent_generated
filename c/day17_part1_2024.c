
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_PROGRAM_SIZE 1024
#define MAX_OUTPUT_SIZE 1024

int A, B, C;
int program[MAX_PROGRAM_SIZE];
int programSize = 0;
char output[MAX_OUTPUT_SIZE];
int outputIndex = 0;

int getComboVal(int op) {
    switch (op) {
        case 1: return 1;
        case 2: return 2;
        case 3: return 3;
        case 4: return A;
        case 5: return B;
        case 6: return C;
        default: return 0;
    }
}


void processLine(char* line) {
    char* token;
    
    if (strncmp(line, "Register A:", 11) == 0) {
        token = strtok(line + 11, ":");
        while(isspace(*token)) token++;
        A = atoi(token);
    } else if (strncmp(line, "Register B:", 11) == 0) {
        token = strtok(line + 11, ":");
        while(isspace(*token)) token++;
        B = atoi(token);
    } else if (strncmp(line, "Register C:", 11) == 0) {
        token = strtok(line + 11, ":");
        while(isspace(*token)) token++;
        C = atoi(token);
    } else if (strncmp(line, "Program:", 8) == 0) {
          token = strtok(line + 8, ":");
         while(isspace(*token)) token++;

         token = strtok(token, ",");
         while (token != NULL && programSize < MAX_PROGRAM_SIZE)
        {
            while(isspace(*token)) token++;
            program[programSize++] = atoi(token);
            token = strtok(NULL, ",");
        }
    }
}

int main() {
    FILE *fp;
    char line[256];
    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
     while (fgets(line, sizeof(line), fp) != NULL) {
        line[strcspn(line, "\r\n")] = 0;
        if (line[0] != '\0') {
            processLine(line);
        }
    }
    fclose(fp);


    int ip = 0;
    while (ip < programSize) {
        int opcode = program[ip];
        if (ip + 1 >= programSize) break;
        int operand = program[ip + 1];

        switch (opcode) {
            case 0: {
                int den = getComboVal(operand);
                if (den != 0) {
                    A /= (1 << den);
                 } else {
                  A = 0;
                }
                 ip += 2;
            }
             break;
            case 1:
                B ^= operand;
                ip += 2;
                break;
            case 2:
                B = getComboVal(operand) % 8;
                ip += 2;
                break;
            case 3:
                if (A != 0) {
                    ip = operand;
                } else {
                    ip += 2;
                }
                break;
            case 4:
                B ^= C;
                ip += 2;
                break;
            case 5: {
                int val = getComboVal(operand) % 8;
                outputIndex += sprintf(output + outputIndex, "%d,", val);
                ip += 2;
            }
              break;
            case 6: {
                int den = getComboVal(operand);
                 B = A / (den !=0 ? (1 << den) : 1);
                 ip += 2;
            }
                break;
            case 7: {
                int den = getComboVal(operand);
                C = A / (den != 0 ? (1 << den) : 1);
                ip += 2;
            }
                break;
            default:
              ip++;
                break;
        }
    }
    if(outputIndex > 0) {
        output[outputIndex - 1] = '\0';
    }
    printf("%s\n", output);

    return 0;
}

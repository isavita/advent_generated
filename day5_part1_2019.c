
#include <stdio.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    int opcode[1000], i = 0;
    
    while(fscanf(fp, "%d,", &opcode[i]) != EOF) {
        i++;
    }
    
    fclose(fp);
    
    int input = 1;
    int output = 0;
    int index = 0;
    
    while (index < i) {
        int instruction = opcode[index];
        int op = instruction % 100;
        int mode1 = (instruction / 100) % 10;
        int mode2 = (instruction / 1000) % 10;
        
        if (op == 1) {
            int param1 = (mode1 == 0) ? opcode[opcode[index + 1]] : opcode[index + 1];
            int param2 = (mode2 == 0) ? opcode[opcode[index + 2]] : opcode[index + 2];
            int param3 = opcode[index + 3];
            opcode[param3] = param1 + param2;
            index += 4;
        } else if (op == 2) {
            int param1 = (mode1 == 0) ? opcode[opcode[index + 1]] : opcode[index + 1];
            int param2 = (mode2 == 0) ? opcode[opcode[index + 2]] : opcode[index + 2];
            int param3 = opcode[index + 3];
            opcode[param3] = param1 * param2;
            index += 4;
        } else if (op == 3) {
            int param1 = opcode[index + 1];
            opcode[param1] = input;
            index += 2;
        } else if (op == 4) {
            int param1 = (mode1 == 0) ? opcode[opcode[index + 1]] : opcode[index + 1];
            output = param1;
            index += 2;
        } else if (op == 99) {
            break;
        } else {
            printf("Unknown opcode: %d\n", op);
            break;
        }
    }
    
    printf("%d\n", output);
    
    return 0;
}

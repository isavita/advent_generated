#include <stdio.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    int data[1000];
    int index = 0;
    
    while (fscanf(file, "%d,", &data[index]) != EOF) {
        index++;
    }
    fclose(file);
    
    int opcode;
    int pos1, pos2, pos3;
    
    data[1] = 12;
    data[2] = 2;
    
    for (int i = 0; i < index; i += 4) {
        opcode = data[i];
        if (opcode == 99) {
            break;
        }
        
        pos1 = data[i + 1];
        pos2 = data[i + 2];
        pos3 = data[i + 3];
        
        if (opcode == 1) {
            data[pos3] = data[pos1] + data[pos2];
        } else if (opcode == 2) {
            data[pos3] = data[pos1] * data[pos2];
        }
    }
    
    printf("%d\n", data[0]);
    
    return 0;
}
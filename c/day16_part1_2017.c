
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void spin(char *programs, int x, int n) {
    char temp[n];
    memcpy(temp, programs, n);
    for (int i = 0; i < n; i++) {
        programs[(i + x) % n] = temp[i];
    }
}

void exchange(char *programs, int a, int b) {
    char temp = programs[a];
    programs[a] = programs[b];
    programs[b] = temp;
}

void partner(char *programs, char a, char b, int n) {
    int indexA, indexB;
    for (int i = 0; i < n; i++) {
        if (programs[i] == a) indexA = i;
        if (programs[i] == b) indexB = i;
    }
    exchange(programs, indexA, indexB);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    
    if ((read = getline(&line, &len, file)) == -1) {
        fclose(file);
        if(line) free(line);
        perror("Error reading line");
        return 1;
    }

    char programs[] = "abcdefghijklmnop";
    int n = strlen(programs);
    char *move = strtok(line, ",");
    while (move != NULL) {
        switch (move[0]) {
            case 's': {
                int x = atoi(move + 1);
                spin(programs, x, n);
                break;
            }
            case 'x': {
               char *pos = move + 1;
                int a = atoi(pos);
                while(*pos != '/') pos++;
                int b = atoi(pos + 1);
                exchange(programs, a, b);
                break;
            }
            case 'p': {
                char a = move[1];
                char b = move[3];
                partner(programs, a, b, n);
                break;
            }
        }
       move = strtok(NULL, ",");
    }
    printf("%s\n", programs);

    fclose(file);
    if(line) free(line);
    return 0;
}

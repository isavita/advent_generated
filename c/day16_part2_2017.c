
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PROGRAMS_LEN 16

void spin(char *programs, int x) {
    char temp[PROGRAMS_LEN];
    memcpy(temp, programs, PROGRAMS_LEN);
    for (int i = 0; i < PROGRAMS_LEN; i++) {
        programs[(i + x) % PROGRAMS_LEN] = temp[i];
    }
}

void exchange(char *programs, int a, int b) {
    char temp = programs[a];
    programs[a] = programs[b];
    programs[b] = temp;
}

void partner(char *programs, char a, char b) {
    int index_a = -1, index_b = -1;
    for (int i = 0; i < PROGRAMS_LEN; i++) {
        if (programs[i] == a) index_a = i;
        if (programs[i] == b) index_b = i;
    }
    if (index_a != -1 && index_b != -1) {
        exchange(programs, index_a, index_b);
    }
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    if ((read = getline(&line, &len, fp)) == -1) {
        perror("Error reading line");
        fclose(fp);
        free(line);
        return 1;
    }
    
    line[strcspn(line, "\n")] = '\0';

    char programs[] = "abcdefghijklmnop";
    char initial[PROGRAMS_LEN + 1];
    strcpy(initial, programs);
    int cycle_len = 0;


    char *moves_copy = strdup(line);
    char *move = strtok(moves_copy, ",");

    for (int i = 0; i < 1000000000; i++) {

        char *current_move = strdup(line);
        char* token = strtok(current_move, ",");

        while(token != NULL){

            if (token[0] == 's') {
                int x = atoi(token + 1);
                spin(programs, x);
            } else if (token[0] == 'x') {
                char *pos = strchr(token, '/');
                if (pos != NULL) {
                    *pos = '\0';
                    int a = atoi(token + 1);
                    int b = atoi(pos + 1);
                    exchange(programs, a, b);
                 }
            } else if (token[0] == 'p') {
                char *pos = strchr(token, '/');
                  if(pos != NULL){
                      *pos = '\0';
                      partner(programs, token[1], pos[1]);
                }

            }
             token = strtok(NULL, ",");

        }
        free(current_move);

         if (strcmp(programs, initial) == 0) {
            cycle_len = i + 1;
            break;
         }
    }
    free(moves_copy);

    strcpy(programs, initial);

    for(int i = 0; i < 1000000000 % cycle_len; i++){
         char *current_move = strdup(line);
         char* token = strtok(current_move, ",");
         while(token != NULL){
            if (token[0] == 's') {
                int x = atoi(token + 1);
                spin(programs, x);
            } else if (token[0] == 'x') {
                char *pos = strchr(token, '/');
                if (pos != NULL) {
                    *pos = '\0';
                    int a = atoi(token + 1);
                    int b = atoi(pos + 1);
                    exchange(programs, a, b);
                 }
            } else if (token[0] == 'p') {
                char *pos = strchr(token, '/');
                  if(pos != NULL){
                      *pos = '\0';
                      partner(programs, token[1], pos[1]);
                }

            }
             token = strtok(NULL, ",");

        }
        free(current_move);


    }
    
    printf("%s\n", programs);

    fclose(fp);
    free(line);

    return 0;
}

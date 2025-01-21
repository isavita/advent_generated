
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    int totalSum = 0;
    
    while ((read = getline(&line, &len, file)) != -1) {
        int gameId = 0;
        char *colon = strchr(line, ':');
        if (!colon) continue;
        
        char* game_str = line + 5;
        char *endptr;
        gameId = strtol(game_str, &endptr, 10);

        char* rounds_str = colon + 2;
        char* round = strtok(rounds_str, ";");
        bool isValid = true;

        while (round != NULL) {
            int red = 0, green = 0, blue = 0;
            char* cube_str = round;
            char* num_ptr = cube_str;
            while(true){
               while (*num_ptr && !isdigit(*num_ptr)){
                   num_ptr++;
               }
                if (!*num_ptr) break;
                char* end_num;
                int count = strtol(num_ptr, &end_num, 10);
                 char* color_ptr = end_num;
                while (*color_ptr && !isalpha(*color_ptr)){
                   color_ptr++;
                }
                if(strncmp(color_ptr,"red",3) == 0) red += count;
                else if (strncmp(color_ptr,"green",5) == 0) green+= count;
                else if (strncmp(color_ptr,"blue",4)==0) blue+=count;
                num_ptr = color_ptr+1;
            }

            if (red > 12 || green > 13 || blue > 14) {
                isValid = false;
                break;
            }
            round = strtok(NULL, ";");
        }

        if (isValid) {
            totalSum += gameId;
        }
    }
    free(line);
    fclose(file);
    printf("%d\n", totalSum);
    return 0;
}

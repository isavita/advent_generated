#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <regex.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) return 1;

    char line[256];
    regex_t regex, cubeRegex;
    regcomp(&regex, "Game ([0-9]+): (.+)", REG_EXTENDED);
    regcomp(&cubeRegex, "([0-9]+) (red|green|blue)", REG_EXTENDED);
    
    int totalPower = 0;

    while (fgets(line, sizeof(line), file)) {
        regmatch_t matches[3];
        if (regexec(&regex, line, 3, matches, 0) == 0) {
            char *rounds = line + matches[2].rm_so;
            int maxRed = 0, maxGreen = 0, maxBlue = 0;

            char *token = strtok(rounds, ";");
            while (token) {
                int red = 0, green = 0, blue = 0;
                char *ptr = token;

                while (regexec(&cubeRegex, ptr, 3, matches, 0) == 0) {
                    char countStr[10];
                    strncpy(countStr, ptr + matches[1].rm_so, matches[1].rm_eo - matches[1].rm_so);
                    countStr[matches[1].rm_eo - matches[1].rm_so] = '\0';
                    int count = atoi(countStr);
                    char color[10];
                    strncpy(color, ptr + matches[2].rm_so, matches[2].rm_eo - matches[2].rm_so);
                    color[matches[2].rm_eo - matches[2].rm_so] = '\0';
                    
                    if (strcmp(color, "red") == 0) red += count;
                    else if (strcmp(color, "green") == 0) green += count;
                    else if (strcmp(color, "blue") == 0) blue += count;

                    ptr += matches[0].rm_eo;
                }

                if (red > maxRed) maxRed = red;
                if (green > maxGreen) maxGreen = green;
                if (blue > maxBlue) maxBlue = blue;

                token = strtok(NULL, ";");
            }

            totalPower += maxRed * maxGreen * maxBlue;
        }
    }

    fclose(file);
    regfree(&regex);
    regfree(&cubeRegex);
    printf("%d\n", totalPower);
    return 0;
}
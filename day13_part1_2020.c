
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    char buffer[255];
    fgets(buffer, sizeof(buffer), file);
    int earliestDeparture = atoi(buffer);

    fgets(buffer, sizeof(buffer), file);
    char *token = strtok(buffer, ",");
    int earliestBusID = 0;
    int minWaitTime = earliestDeparture;

    while (token != NULL) {
        if (strcmp(token, "x") == 0) {
            token = strtok(NULL, ",");
            continue;
        }
        int busID = atoi(token);
        int waitTime = busID - (earliestDeparture % busID);
        if (waitTime < minWaitTime) {
            minWaitTime = waitTime;
            earliestBusID = busID;
        }
        token = strtok(NULL, ",");
    }

    printf("%d\n", earliestBusID * minWaitTime);

    fclose(file);
    return 0;
}

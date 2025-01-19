
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_MESSAGES 1000
#define MAX_LENGTH 1000

char messages[MAX_MESSAGES][MAX_LENGTH + 1];
int messageCount = 0;

char getLeastCommonChar(int *count) {
    char minChar = 0;
    int minCount = INT_MAX;
    for (int i = 0; i < 256; i++) {
        if (count[i] > 0 && count[i] < minCount) {
            minCount = count[i];
            minChar = (char)i;
        }
    }
    return minChar;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    while (fgets(messages[messageCount], MAX_LENGTH + 1, file) != NULL && messageCount < MAX_MESSAGES) {
      messages[messageCount][strcspn(messages[messageCount], "\n")] = 0;
        messageCount++;
    }

    fclose(file);

    if (messageCount == 0) {
        printf("\n");
        return 0;
    }

    int messageLength = strlen(messages[0]);
    char originalMessage[messageLength + 1];
    
    for (int j = 0; j < messageLength; j++) {
      int count[256] = {0};
      for (int i = 0; i < messageCount; i++){
        count[(int)messages[i][j]]++;
      }
        originalMessage[j] = getLeastCommonChar(count);
    }
    originalMessage[messageLength] = '\0';
    printf("%s\n", originalMessage);

    return 0;
}

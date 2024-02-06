
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define INPUT_FILE "input.txt"
#define MAX_LINE_LENGTH 1024

int isRealRoom(char* room, char* checksum);
int getSectorID(char* room);
void decryptName(char* room, int sectorID, char* decryptedName);

int main() {
    FILE* file = fopen(INPUT_FILE, "r");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0; // Remove newline character
        char checksum[6];
        if (isRealRoom(line, checksum)) {
            int sectorID = getSectorID(line);
            char decryptedName[MAX_LINE_LENGTH];
            decryptName(line, sectorID, decryptedName);
            if (strstr(decryptedName, "northpole object")) {
                printf("%d\n", sectorID);
                break;
            }
        }
    }
    fclose(file);
    return 0;
}

int isRealRoom(char* room, char* checksum) {
    char* bracket = strchr(room, '[');
    if (!bracket) return 0;
    strncpy(checksum, bracket + 1, 5);
    checksum[5] = '\0'; // Ensure string is null-terminated

    int letterCounts[26] = {0};
    for (char* p = room; p < bracket; ++p) {
        if (isalpha(*p)) {
            letterCounts[*p - 'a']++;
        }
    }

    for (int i = 0; i < 5; ++i) {
        int max = 0, maxIndex = -1;
        for (int j = 0; j < 26; ++j) {
            if (letterCounts[j] > max) {
                max = letterCounts[j];
                maxIndex = j;
            }
        }
        if (maxIndex != checksum[i] - 'a') return 0;
        letterCounts[maxIndex] = 0; // Remove this letter from consideration
    }

    return 1;
}

int getSectorID(char* room) {
    char* dash = strrchr(room, '-');
    if (!dash) return 0;
    return atoi(dash + 1);
}

void decryptName(char* room, int sectorID, char* decryptedName) {
    char* dash = NULL;
    int i = 0;
    while ((dash = strchr(room + i, '-')) != NULL) {
        int len = dash - room - i;
        strncpy(decryptedName + i, room + i, len);
        decryptedName[i + len] = ' ';
        i += len + 1;
    }

    decryptedName[i - 1] = '\0'; // Replace the last space with null-terminator

    for (char* p = decryptedName; *p; ++p) {
        if (isalpha(*p)) {
            *p = 'a' + ((*p - 'a' + sectorID) % 26);
        }
    }
}

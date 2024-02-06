
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LENGTH 100

typedef struct {
    char name[100];
    int sectorID;
    char checksum[6];
} Room;

int compare(const void *a, const void *b) {
    return (*(int*)b) - (*(int*)a);
}

int isRealRoom(const Room* room) {
    int letters[26] = {0};
    for (int i = 0; room->name[i]; ++i) {
        if (room->name[i] >= 'a' && room->name[i] <= 'z') {
            letters[room->name[i] - 'a']++;
        }
    }
    
    int maxValues[5] = {0};
    char maxLetters[5] = {'\0'};
    for (int i = 0; i < 5; ++i) {
        int maxIndex = 0;
        for (int j = 1; j < 26; ++j) {
            if (letters[j] > letters[maxIndex] || (letters[j] == letters[maxIndex] && j < maxIndex)) {
                maxIndex = j;
            }
        }
        maxValues[i] = letters[maxIndex];
        maxLetters[i] = 'a' + maxIndex;
        letters[maxIndex] = 0;
    }
    
    return strncmp(room->checksum, maxLetters, 5) == 0;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        printf("Error opening file\n");
        return 1;
    }
    
    char line[MAX_LINE_LENGTH];
    int totalSectorID = 0;
    while (fgets(line, sizeof(line), fp)) {
        Room room;
        sscanf(line, "%[^0-9]%d[%5s]", room.name, &room.sectorID, room.checksum);
        room.checksum[5] = '\0'; // Ensure null-termination
        
        if (isRealRoom(&room)) {
            totalSectorID += room.sectorID;
        }
    }
    
    fclose(fp);
    printf("%d\n", totalSectorID);
    return 0;
}

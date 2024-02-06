
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* hexToBin(const char* hex) {
    char* bin = malloc(strlen(hex) * 4 + 1); // Each hex digit corresponds to 4 binary digits
    if (bin == NULL) {
        perror("Malloc failed");
        exit(EXIT_FAILURE);
    }
    bin[0] = '\0';

    for (int i = 0; hex[i] != '\0'; i++) {
        switch (hex[i]) {
            case '0': strcat(bin, "0000"); break;
            case '1': strcat(bin, "0001"); break;
            case '2': strcat(bin, "0010"); break;
            case '3': strcat(bin, "0011"); break;
            case '4': strcat(bin, "0100"); break;
            case '5': strcat(bin, "0101"); break;
            case '6': strcat(bin, "0110"); break;
            case '7': strcat(bin, "0111"); break;
            case '8': strcat(bin, "1000"); break;
            case '9': strcat(bin, "1001"); break;
            case 'A': case 'a': strcat(bin, "1010"); break;
            case 'B': case 'b': strcat(bin, "1011"); break;
            case 'C': case 'c': strcat(bin, "1100"); break;
            case 'D': case 'd': strcat(bin, "1101"); break;
            case 'E': case 'e': strcat(bin, "1110"); break;
            case 'F': case 'f': strcat(bin, "1111"); break;
            default: 
                printf("Invalid hex digit %c\n", hex[i]);
                free(bin);
                exit(EXIT_FAILURE);
        }
    }
    return bin;
}

int parsePacket(const char* binStr, int idx, int* versionSum) {
    int version = (binStr[idx]-'0')<<2 | (binStr[idx+1]-'0')<<1 | (binStr[idx+2]-'0');
    int typeID = (binStr[idx+3]-'0')<<2 | (binStr[idx+4]-'0')<<1 | (binStr[idx+5]-'0');
    idx += 6;

    *versionSum += version;

    if (typeID == 4) {
        while (binStr[idx] == '1') {
            idx += 5;
        }
        idx += 5;
        return idx;
    }

    int lengthTypeID = binStr[idx] - '0';
    idx++;
    int subPacketLength = 0, numSubPackets = 0;

    if (lengthTypeID == 0) {
        for (int i = 0; i < 15; i++) {
            subPacketLength = subPacketLength<<1 | (binStr[idx]-'0');
            idx++;
        }
    } else {
        for (int i = 0; i < 11; i++) {
            numSubPackets = numSubPackets<<1 | (binStr[idx]-'0');
            idx++;
        }
    }

    if (lengthTypeID == 0) {
        int startIdx = idx;
        while (idx - startIdx < subPacketLength) {
            idx = parsePacket(binStr, idx, versionSum);
        }
    } else {
        for (int i = 0; i < numSubPackets; i++) {
            idx = parsePacket(binStr, idx, versionSum);
        }
    }
    return idx;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);
    char* hexStr = malloc(fileSize + 1);
    if (hexStr == NULL) {
        perror("Malloc failed");
        return EXIT_FAILURE;
    }
    fread(hexStr, 1, fileSize, file);
    fclose(file);
    hexStr[fileSize] = '\0';

    char* binStr = hexToBin(hexStr);
    free(hexStr);

    int versionSum = 0;
    parsePacket(binStr, 0, &versionSum);
    printf("%d\n", versionSum);

    free(binStr);
    return 0;
}

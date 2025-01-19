
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>

char *hexToBin(const char *hex) {
    size_t hexLen = strlen(hex);
    char *bin = malloc(hexLen * 4 + 1);
    if (!bin) {
        perror("malloc failed");
        exit(EXIT_FAILURE);
    }
    bin[0] = '\0';

    for (size_t i = 0; i < hexLen; ++i) {
        char h = hex[i];
        int val;
        if (isdigit(h)) {
            val = h - '0';
        } else if (h >= 'a' && h <= 'f') {
            val = h - 'a' + 10;
        } else if (h >= 'A' && h <= 'F') {
            val = h - 'A' + 10;
        } else {
            fprintf(stderr, "Invalid hex character: %c\n", h);
            free(bin);
            exit(EXIT_FAILURE);
        }

        for (int j = 3; j >= 0; --j) {
            bin[i * 4 + (3 - j)] = ((val >> j) & 1) + '0';
        }
    }
    bin[hexLen * 4] = '\0';
    return bin;
}

typedef struct {
    int version;
    int idx;
    int64_t value;
} PacketResult;

PacketResult parsePacket(const char *binStr, int idx) {
    PacketResult result;
    result.version = (binStr[idx] - '0') << 2 | (binStr[idx + 1] - '0') << 1 | (binStr[idx + 2] - '0');
    int typeID = (binStr[idx + 3] - '0') << 2 | (binStr[idx + 4] - '0') << 1 | (binStr[idx + 5] - '0');
    idx += 6;

    if (typeID == 4) {
        int64_t value = 0;
        while (binStr[idx] == '1') {
            value = (value << 4) | ((int64_t)(binStr[idx + 1] - '0') << 3) | ((int64_t)(binStr[idx + 2] - '0') << 2) | ((int64_t)(binStr[idx + 3] - '0') << 1) | (binStr[idx + 4] - '0');
            idx += 5;
        }
        value = (value << 4) | ((int64_t)(binStr[idx + 1] - '0') << 3) | ((int64_t)(binStr[idx + 2] - '0') << 2) | ((int64_t)(binStr[idx + 3] - '0') << 1) | (binStr[idx + 4] - '0');
        idx += 5;
        result.idx = idx;
        result.value = value;
        return result;
    }

    int lengthTypeID = binStr[idx] - '0';
    idx++;
    int numSubPackets = 0;
    int subPacketLength = 0;

    if (lengthTypeID == 0) {
         for (int i = 0; i < 15; i++) {
             subPacketLength = (subPacketLength << 1) | (binStr[idx] - '0');
             idx++;
        }
    } else {
        for (int i = 0; i < 11; i++) {
            numSubPackets = (numSubPackets << 1) | (binStr[idx] - '0');
            idx++;
        }
    }
    
    int64_t values[1024];
    int valueCount = 0;
    while (1) {
        if (lengthTypeID == 0 && subPacketLength == 0) break;
        if (lengthTypeID == 1 && numSubPackets == 0) break;

        PacketResult subResult = parsePacket(binStr, idx);
        values[valueCount++] = subResult.value;
        if (lengthTypeID == 0) {
            subPacketLength -= subResult.idx - idx;
        } else {
             numSubPackets--;
        }
        idx = subResult.idx;
    }
    
    int64_t res = 0;
    switch (typeID) {
        case 0:
            for (int i=0; i<valueCount; i++){
                res += values[i];
            }
            break;
        case 1:
            res = 1;
            for (int i=0; i<valueCount; i++){
                res *= values[i];
            }
            break;
        case 2:
            res = values[0];
            for (int i=1; i<valueCount; i++){
                if (values[i] < res) res = values[i];
            }
            break;
        case 3:
             res = values[0];
            for (int i=1; i<valueCount; i++){
                 if (values[i] > res) res = values[i];
            }
            break;
        case 5:
            res = values[0] > values[1] ? 1 : 0;
            break;
        case 6:
             res = values[0] < values[1] ? 1 : 0;
             break;
        case 7:
            res = values[0] == values[1] ? 1 : 0;
            break;
        default:
            fprintf(stderr, "Unknown typeID: %d\n", typeID);
            exit(EXIT_FAILURE);
    }
   
    result.idx = idx;
    result.value = res;
    return result;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }
    
    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    
    char *hexStr = malloc(fsize + 1);
    if (!hexStr){
        perror("malloc failed");
        fclose(fp);
        return EXIT_FAILURE;
    }
    
    fread(hexStr, 1, fsize, fp);
    hexStr[fsize] = '\0';
    fclose(fp);
    
    size_t len = strlen(hexStr);
    while(len > 0 && isspace(hexStr[len -1])) {
        hexStr[--len] = '\0';
    }

    char *binStr = hexToBin(hexStr);
    free(hexStr);
    
    PacketResult finalResult = parsePacket(binStr, 0);
    free(binStr);
    printf("%lld\n", finalResult.value);
    return EXIT_SUCCESS;
}


#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <ctype.h>

#define INPUT_FILENAME "input.txt"
#define FREE_SPACE -1

int main(int argc, const char * argv[]) {
    FILE *fp = fopen(INPUT_FILENAME, "r");
    if (!fp) return 1;

    long totalSize = 0;
    int c;
    while ((c = fgetc(fp)) != EOF && c != '\n')
        if (isdigit(c)) totalSize += c - '0';

    if (totalSize == 0) {
        printf("0\n");
        fclose(fp);
        return 0;
    }

    int *disk = malloc(totalSize * sizeof(int));
    if (!disk) { fclose(fp); return 1; }

    rewind(fp);
    long pos = 0;
    int idx = 0;
    while ((c = fgetc(fp)) != EOF && c != '\n') {
        if (isdigit(c)) {
            int len = c - '0';
            if (idx % 2 == 0) {
                int fid = idx / 2;
                for (int i = 0; i < len; ++i) disk[pos++] = fid;
            } else {
                for (int i = 0; i < len; ++i) disk[pos++] = FREE_SPACE;
            }
            idx++;
        }
    }
    fclose(fp);

    long left = 0, right = totalSize - 1;
    while (left < right) {
        while (left < right && disk[left] != FREE_SPACE) left++;
        while (left < right && disk[right] == FREE_SPACE) right--;
        if (left < right) {
            disk[left] = disk[right];
            disk[right] = FREE_SPACE;
            left++; right--;
        }
    }

    unsigned long long checksum = 0;
    for (long i = 0; i < totalSize; ++i)
        if (disk[i] != FREE_SPACE) checksum += (unsigned long long)i * disk[i];

    printf("%llu\n", checksum);
    free(disk);
    return 0;
}

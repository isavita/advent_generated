
#import <Foundation/Foundation.h>

typedef struct {
    char name[100];
    int sectorID;
    char checksum[6];
} Room;

static BOOL isRealRoom(const Room *r) {
    int letters[26] = {0};
    for (int i = 0; r->name[i]; ++i) {
        char c = r->name[i];
        if (c >= 'a' && c <= 'z') letters[c - 'a']++;
    }
    char top[5];
    for (int k = 0; k < 5; ++k) {
        int idx = 0;
        for (int j = 1; j < 26; ++j) {
            if (letters[j] > letters[idx] || (letters[j] == letters[idx] && j < idx))
                idx = j;
        }
        top[k] = 'a' + idx;
        letters[idx] = -1;
    }
    return strncmp(r->checksum, top, 5) == 0;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSUInteger total = 0;
        for (NSString *line in [data componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]]) {
            if (line.length == 0) continue;
            Room room = {0};
            sscanf([line UTF8String], "%[^0-9]%d[%5s]", room.name, &room.sectorID, room.checksum);
            if (isRealRoom(&room)) total += room.sectorID;
        }
        printf("%lu\n", (unsigned long)total);
    }
    return 0;
}

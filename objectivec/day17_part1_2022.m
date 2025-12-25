
#import <Foundation/Foundation.h>

#define ROCK_COUNT 5
#define ROCK_0_COUNT 4
#define ROCK_1_COUNT 5
#define ROCK_2_COUNT 5
#define ROCK_3_COUNT 4
#define ROCK_4_COUNT 4

static const char rock0[ROCK_0_COUNT][2] = {{0,0},{1,0},{2,0},{3,0}};
static const char rock1[ROCK_1_COUNT][2] = {{1,0},{0,1},{1,1},{2,1},{1,2}};
static const char rock2[ROCK_2_COUNT][2] = {{0,0},{1,0},{2,0},{2,1},{2,2}};
static const char rock3[ROCK_3_COUNT][2] = {{0,0},{0,1},{0,2},{0,3}};
static const char rock4[ROCK_4_COUNT][2] = {{0,0},{1,0},{0,1},{1,1}};

static const char* rocks[ROCK_COUNT] = {
    (const char*)rock0, (const char*)rock1, (const char*)rock2, (const char*)rock3, (const char*)rock4
};
static const char rockSizes[ROCK_COUNT] = {ROCK_0_COUNT, ROCK_1_COUNT, ROCK_2_COUNT, ROCK_3_COUNT, ROCK_4_COUNT};

static BOOL blocked(NSMutableData* chamber, int x, int y) {
    if (x < 0 || x >= 7 || y < 0) return YES;
    int w = 7;
    int bytesPerRow = (w + 7) / 8;
    int byteIndex = y * bytesPerRow + (x / 8);
    int bitIndex = x % 8;
    if (byteIndex >= (int)[chamber length]) return NO;
    char byte = ((char*)[chamber mutableBytes])[byteIndex];
    return (byte & (1 << bitIndex)) != 0;
}

static void setBlocked(NSMutableData* chamber, int x, int y) {
    int w = 7;
    int bytesPerRow = (w + 7) / 8;
    int byteIndex = y * bytesPerRow + (x / 8);
    int bitIndex = x % 8;
    int needed = byteIndex + 1;
    if (needed > (int)[chamber length]) {
        [chamber increaseLengthBy:needed - [chamber length]];
    }
    char* bytes = (char*)[chamber mutableBytes];
    bytes[byteIndex] |= (1 << bitIndex);
}

int main(int argc, char* argv[]) {
    @autoreleasepool {
        NSError* error = nil;
        NSString* jets = [NSString stringWithContentsOfFile:@"input.txt"
                                                encoding:NSUTF8StringEncoding
                                                   error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }
        jets = [jets stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
        NSUInteger jetLen = [jets length];
        if (jetLen == 0) return 0;

        NSMutableData* chamber = [NSMutableData data];
        int height = 0;
        NSUInteger jetIndex = 0;
        char rockIndex = 0;

        for (int n = 0; n < 2022; n++) {
            char size = rockSizes[rockIndex];
            const char* rock = (const char*)rocks[rockIndex];
            rockIndex = (rockIndex + 1) % ROCK_COUNT;

            int rx[5], ry[5];
            for (char i = 0; i < size; i++) {
                rx[i] = rock[i*2] + 2;
                ry[i] = rock[i*2+1] + height + 3;
            }

            BOOL settled = NO;
            while (!settled) {
                unichar jetCh = [jets characterAtIndex:jetIndex];
                jetIndex = (jetIndex + 1) % jetLen;
                int dx = (jetCh == '<') ? -1 : 1;

                BOOL canMove = YES;
                for (char i = 0; i < size; i++) {
                    if (blocked(chamber, rx[i] + dx, ry[i])) {
                        canMove = NO;
                        break;
                    }
                }
                if (canMove) {
                    for (char i = 0; i < size; i++) rx[i] += dx;
                }

                canMove = YES;
                for (char i = 0; i < size; i++) {
                    if (blocked(chamber, rx[i], ry[i] - 1)) {
                        canMove = NO;
                        break;
                    }
                }
                if (!canMove) {
                    for (char i = 0; i < size; i++) setBlocked(chamber, rx[i], ry[i]);
                    int maxY = 0;
                    for (char i = 0; i < size; i++) {
                        if (ry[i] + 1 > maxY) maxY = ry[i] + 1;
                    }
                    if (maxY > height) height = maxY;
                    settled = YES;
                } else {
                    for (char i = 0; i < size; i++) ry[i]--;
                }
            }
        }

        printf("%d\n", height);
        return 0;
    }
}

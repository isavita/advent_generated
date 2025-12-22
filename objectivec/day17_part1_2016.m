#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonDigest.h>

#define CAP 32768
#define MAX 100

typedef struct {
    int x, y, len;
    char path[MAX];
} State;

int main(int argc, char *argv[]) {
    @autoreleasepool {
        NSString *pass = [NSString stringWithContentsOfFile:@"input.txt"
                                                     encoding:NSUTF8StringEncoding
                                                        error:nil];
        if (!pass) return 1;
        pass = [pass stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSUInteger baseLen = pass.length;
        char base[baseLen + 1];
        [pass getCString:base maxLength:sizeof(base) encoding:NSUTF8StringEncoding];

        State q[CAP];
        int head = 0, tail = 0;
        q[tail++] = (State){0, 0, 0, ""};

        char in[baseLen + MAX];
        memcpy(in, base, baseLen);

        char dir[] = "UDLR";
        int dx[] = {0, 0, -1, 1};
        int dy[] = {-1, 1, 0, 0};

        while (head != tail) {
            State cur = q[head];
            head = (head + 1) & (CAP - 1);

            if (cur.x == 3 && cur.y == 3) {
                printf("%s\n", cur.path);
                return 0;
            }

            memcpy(in + baseLen, cur.path, cur.len);
            in[baseLen + cur.len] = '\0';

            unsigned char hash[CC_MD5_DIGEST_LENGTH];
            CC_MD5(in, (CC_LONG)(baseLen + cur.len), hash);

            for (int i = 0; i < 4; ++i) {
                unsigned char n = (i & 1) ? (hash[i / 2] & 0x0F) : (hash[i / 2] >> 4);
                if (n > 10) {
                    int nx = cur.x + dx[i], ny = cur.y + dy[i];
                    if (nx >= 0 && nx < 4 && ny >= 0 && ny < 4) {
                        State *nxt = &q[tail];
                        tail = (tail + 1) & (CAP - 1);
                        *nxt = (State){nx, ny, cur.len + 1, ""};
                        memcpy(nxt->path, cur.path, cur.len);
                        nxt->path[cur.len] = dir[i];
                        nxt->path[cur.len + 1] = '\0';
                    }
                }
            }
        }
        return 1;
    }
}
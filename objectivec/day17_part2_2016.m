
#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonCrypto.h>

@interface PointObj : NSObject
@property int x, y;
@property (nonatomic, strong) NSString *path;
+ (instancetype)pointWithX:(int)x y:(int)y path:(NSString *)path;
@end

@implementation PointObj
+ (instancetype)pointWithX:(int)x y:(int)y path:(NSString *)path {
    PointObj *p = [self new];
    p.x = x; p.y = y; p.path = path;
    return p;
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *passcode = [NSString stringWithContentsOfFile:@"input.txt"
                                                      encoding:NSUTF8StringEncoding
                                                         error:nil];
        passcode = [passcode stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray *queue = [NSMutableArray array];
        [queue addObject:[PointObj pointWithX:0 y:0 path:@""]];
        NSUInteger idx = 0;
        int longest = 0;
        const int dx[4] = {0, 0, -1, 1};
        const int dy[4] = {-1, 1, 0, 0};
        const char dirs[5] = "UDLR";
        while (idx < queue.count) {
            PointObj *cur = queue[idx++];
            if (cur.x == 3 && cur.y == 3) {
                longest = (int)MAX(longest, cur.path.length);
                continue;
            }
            NSString *input = [passcode stringByAppendingString:cur.path];
            const char *cstr = [input UTF8String];
            unsigned char digest[CC_MD5_DIGEST_LENGTH];
            CC_MD5(cstr, (CC_LONG)strlen(cstr), digest);
            char hex[33];
            for (int i = 0; i < 16; ++i) sprintf(hex + i * 2, "%02x", digest[i]);
            for (int i = 0; i < 4; ++i) {
                char d = hex[i];
                if (d >= 'b' && d <= 'f') {
                    int nx = cur.x + dx[i];
                    int ny = cur.y + dy[i];
                    if (nx >= 0 && nx < 4 && ny >= 0 && ny < 4) {
                        NSString *np = [cur.path stringByAppendingFormat:@"%c", dirs[i]];
                        [queue addObject:[PointObj pointWithX:nx y:ny path:np]];
                    }
                }
            }
        }
        printf("%d\n", longest);
    }
    return 0;
}

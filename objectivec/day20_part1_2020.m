#import <Foundation/Foundation.h>

#define TILE_DIM 10
#define MAX_TILES 144

@interface Tile : NSObject
@property (assign) int tid;
@property (strong) NSMutableArray<NSString*> *rows;
- (instancetype)initWithID:(int)tid;
- (void)addRow:(NSString*)r;
- (unichar)pAtRow:(int)r col:(int)c;
- (NSString*)edge:(int)side;           // 0 top 1 bottom 2 left 3 right
- (NSString*)edgeReversed:(int)side;
@end

@implementation Tile
- (instancetype)initWithID:(int)tid {
    self = [super init];
    if (self) { _tid = tid; _rows = [NSMutableArray arrayWithCapacity:TILE_DIM]; }
    return self;
}
- (void)addRow:(NSString*)r { [_rows addObject:r]; }
- (unichar)pAtRow:(int)r col:(int)c { return [_rows[r] characterAtIndex:c]; }
- (NSString*)edge:(int)side {
    if (side==0) return _rows[0];
    if (side==1) return _rows[TILE_DIM-1];
    if (side==2) {
        NSMutableString *s = [NSMutableString stringWithCapacity:TILE_DIM];
        for (int i=0;i<TILE_DIM;i++) [s appendFormat:@"%C",[self pAtRow:i col:0]];
        return s;
    }
    NSMutableString *s = [NSMutableString stringWithCapacity:TILE_DIM];
    for (int i=0;i<TILE_DIM;i++) [s appendFormat:@"%C",[self pAtRow:i col:TILE_DIM-1]];
    return s;
}
- (NSString*)edgeReversed:(int)side {
    NSString *e = [self edge:side];
    NSUInteger l = e.length;
    unichar buf[l];
    [e getCharacters:buf range:NSMakeRange(0, l)];
    for (NSUInteger i=0;i<l/2;i++){ unichar t=buf[i]; buf[i]=buf[l-1-i]; buf[l-1-i]=t; }
    return [NSString stringWithCharacters:buf length:l];
}
@end

static NSArray<Tile*>* loadTiles(NSString *path) {
    NSError *err = nil;
    NSString *whole = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];
    if (!whole) { NSLog(@"%@",err); exit(1); }
    NSMutableArray<Tile*> *tiles = [NSMutableArray array];
    NSArray<NSString*> *chunks = [whole componentsSeparatedByString:@"Tile "];
    for (NSString *chunk in chunks) {
        if (chunk.length==0) continue;
        NSArray<NSString*> *lines = [chunk componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        int tid = [[lines[0] stringByTrimmingCharactersInSet:[NSCharacterSet punctuationCharacterSet]] intValue];
        Tile *t = [[Tile alloc] initWithID:tid];
        for (int i=1;i<lines.count;i++) {
            NSString *trim = [lines[i] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if (trim.length==TILE_DIM) [t addRow:trim];
        }
        [tiles addObject:t];
    }
    return tiles;
}

int main(int argc, char *argv[]) {
    @autoreleasepool {
        NSArray<Tile*> *tiles = loadTiles(@"input.txt");
        NSMutableDictionary<NSString*,NSNumber*> *cnt = [NSMutableDictionary dictionary];
        for (Tile *t in tiles) {
            for (int s=0;s<4;s++) {
                NSString *e1 = [t edge:s];
                NSString *e2 = [t edgeReversed:s];
                cnt[e1] = @(cnt[e1].intValue + 1);
                cnt[e2] = @(cnt[e2].intValue + 1);
            }
        }
        unsigned long long prod = 1;
        for (Tile *t in tiles) {
            int uniq = 0;
            for (int s=0;s<4;s++) {
                NSString *e = [t edge:s];
                if (cnt[e].intValue==1) uniq++;
            }
            if (uniq==2) prod *= t.tid;
        }
        printf("%llu\n",prod);
    }
    return 0;
}
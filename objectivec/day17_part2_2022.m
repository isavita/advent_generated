
#import <Foundation/Foundation.h>

#define WIDTH 7
#define PROFILE_DEPTH 30
#define TOTAL_ROCKS 1000000000000LL

static const int rockCount[5] = {4,5,5,4,4};
static const int rockShapes[5][5][2] = {
    {{0,0},{1,0},{2,0},{3,0}},
    {{1,0},{0,1},{1,1},{2,1},{1,2}},
    {{0,0},{1,0},{2,0},{2,1},{2,2}},
    {{0,0},{0,1},{0,2},{0,3}},
    {{0,0},{1,0},{0,1},{1,1}}
};

static BOOL tryMove(int rock[5][2], int cnt, int dx, int dy, NSMutableDictionary *chamber, long long *high) {
    int nx[5], ny[5];
    for (int i=0;i<cnt;i++) {
        int x = rock[i][0]+dx, y = rock[i][1]+dy;
        if (x<0||x>=WIDTH||y<=0) return NO;
        NSNumber *mask = chamber[@(y)];
        unsigned char m = mask? [mask unsignedCharValue] : 0;
        if (m & (1<<x)) return NO;
        nx[i]=x; ny[i]=y;
    }
    for (int i=0;i<cnt;i++) {rock[i][0]=nx[i]; rock[i][1]=ny[i];}
    return YES;
}

static void chamberSet(NSMutableDictionary *chamber, long long y, unsigned char mask) {
    chamber[@(y)] = @(mask);
}

static unsigned char chamberGet(NSMutableDictionary *chamber, long long y) {
    NSNumber *n = chamber[@(y)];
    return n? [n unsignedCharValue] : 0;
}

static void profile(NSMutableDictionary *chamber, long long high, int prof[WIDTH]) {
    for (int x=0;x<WIDTH;x++) {
        prof[x]=PROFILE_DEPTH;
        for (int d=0;d<PROFILE_DEPTH;d++) {
            long long y=high-d;
            if (y<=0) break;
            if (chamberGet(chamber,y)&(1<<x)) {prof[x]=d;break;}
        }
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool{
        NSString *path = @"input.txt";
        NSString *jet = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        jet = [jet stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        int jetLen = (int)jet.length;
        if (!jetLen) return 1;
        NSMutableDictionary *chamber = [NSMutableDictionary dictionary];
        NSMutableDictionary *states = [NSMutableDictionary dictionary];
        long long high=0, jetIdx=0, rockNum=0, extra=0;
        BOOL cycleDone=NO;
        while (rockNum<TOTAL_ROCKS) {
            int rIdx = rockNum%5;
            int cnt = rockCount[rIdx];
            int rock[5][2];
            long long startY = high+4;
            for (int i=0;i<cnt;i++) {
                rock[i][0]=rockShapes[rIdx][i][0]+2;
                rock[i][1]=rockShapes[rIdx][i][1]+startY;
            }
            while (1) {
                unichar dir = [jet characterAtIndex:(jetIdx%jetLen)];
                jetIdx++;
                if (dir=='>') tryMove(rock,cnt,1,0,chamber,&high);
                else if (dir=='<') tryMove(rock,cnt,-1,0,chamber,&high);
                if (!tryMove(rock,cnt,0,-1,chamber,&high)) {
                    for (int i=0;i<cnt;i++) {
                        long long y=rock[i][1];
                        unsigned char m=chamberGet(chamber,y);
                        chamberSet(chamber,y,m|(1<<rock[i][0]));
                        if (y>high) high=y;
                    }
                    break;
                }
            }
            if (!cycleDone && rockNum>jetLen) {
                int prof[WIDTH];
                profile(chamber,high,prof);
                NSMutableString *key=[NSMutableString stringWithFormat:@"%d-%lld",rIdx,jetIdx%jetLen];
                for (int i=0;i<WIDTH;i++) [key appendFormat:@"-%d",prof[i]];
                NSDictionary *prev=states[key];
                if (prev) {
                    long long prevRock=[prev[@"rock"] longLongValue];
                    long long prevHigh=[prev[@"high"] longLongValue];
                    long long cycleR=rockNum-prevRock;
                    long long cycleH=high-prevHigh;
                    long long remain=TOTAL_ROCKS-rockNum-1;
                    long long cycles=remain/cycleR;
                    if (cycles) {
                        extra+=cycles*cycleH;
                        rockNum+=cycles*cycleR;
                        cycleDone=YES;
                    }
                }else{
                    states[key]=@{@"rock":@(rockNum),@"high":@(high)};
                }
            }
            rockNum++;
        }
        printf("%lld\n",high+extra);
    }
    return 0;
}

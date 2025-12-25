
#import <Foundation/Foundation.h>

int H, W;
char **graph;
int moves[4][2] = {{-1,0},{0,-1},{1,0},{0,1}};

void addOuter(int labelIndex, NSMutableDictionary<NSString*, NSMutableSet<NSNumber*>*> *side, int x, int y) {
    NSString *label = labelIndex==0 ? @"left" : labelIndex==1 ? @"up" : labelIndex==2 ? @"right" : @"down";
    int key = (labelIndex==1||labelIndex==3) ? y*10000+x : x*10000+y;
    [side[label] addObject:@(key)];
}

int countOuter(NSMutableDictionary<NSString*, NSMutableSet<NSNumber*>*> *side) {
    int outer = 0;
    for (NSString *label in side) {
        NSArray<NSNumber*> *sorted = [[side[label] allObjects] sortedArrayUsingSelector:@selector(compare:)];
        NSMutableArray<NSNumber*> *temp = [NSMutableArray array];
        for (NSNumber *keyObj in sorted) {
            int key = [keyObj intValue];
            int i = key/10000, j = key%10000;
            BOOL found = NO;
            for (int d = -1; d <= 1; d += 2) {
                int neighbor = (i)*10000 + (j+d);
                if ([temp containsObject:@(neighbor)]) { found=YES; break; }
            }
            if (!found) outer++;
            [temp addObject:keyObj];
        }
    }
    return outer;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString*> *lines = [txt componentsSeparatedByString:@"\n"];
        NSMutableArray<NSString*> *clean = [NSMutableArray array];
        for (NSString *L in lines) {
            NSString *t = [L stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            if ([t length]) [clean addObject:t];
        }
        H = (int)[clean count];
        W = (int)[clean[0] length];
        graph = malloc(H*sizeof(char*));
        for (int i=0;i<H;i++){
            graph[i] = malloc(W+1);
            strcpy(graph[i], [clean[i] cStringUsingEncoding:NSASCIIStringEncoding]);
        }

        long totalSum = 0;
        for (int y=0;y<H;y++){
            for (int x=0;x<W;x++){
                if (graph[y][x]=='.') continue;
                int area=0;
                char target=graph[y][x];
                NSMutableSet<NSNumber*> *visited = [NSMutableSet set];
                NSMutableDictionary<NSString*, NSMutableSet<NSNumber*>*> *side = [@{
                    @"left": [NSMutableSet set],
                    @"up": [NSMutableSet set],
                    @"right": [NSMutableSet set],
                    @"down": [NSMutableSet set]
                } mutableCopy];

                NSMutableArray<NSArray*> *q = [NSMutableArray array];
                [q addObject: @[@(x),@(y),@(-1)]];
                while ([q count]){
                    NSArray *cur = q[0]; [q removeObjectAtIndex:0];
                    int cx=[cur[0] intValue], cy=[cur[1] intValue], label=[cur[2] intValue];
                    if (graph[cy][cx]!=target){
                        if (label!=-1 && ![visited containsObject:@(cx*W+cy)]) addOuter(label,side,cx,cy);
                        continue;
                    }
                    [visited addObject:@(cx*W+cy)];
                    area++;
                    graph[cy][cx]='.';
                    for (int m=0;m<4;m++){
                        int nx=cx+moves[m][0], ny=cy+moves[m][1];
                        if (nx>=0&&nx<W&&ny>=0&&ny<H) [q addObject: @[@(nx),@(ny),@(m)]];
                        else addOuter(m,side,nx,ny);
                    }
                }
                totalSum += (long)area * countOuter(side);
            }
        }
        printf("%ld\n", totalSum);
        for (int i=0;i<H;i++) free(graph[i]);
        free(graph);
    }
    return 0;
}

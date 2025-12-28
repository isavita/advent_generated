
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                       encoding:NSUTF8StringEncoding
                                                          error:nil];
        if (!content) {
            printf("Largest valid area: 0\n");
            return 0;
        }
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        NSMutableArray *ptsX = [NSMutableArray array];
        NSMutableArray *ptsY = [NSMutableArray array];
        for (NSString *line in lines) {
            NSArray *parts = [[line stringByReplacingOccurrencesOfString:@" " withString:@""]
                              componentsSeparatedByString:@","];
            if (parts.count == 2) {
                int x = [parts[0] intValue];
                int y = [parts[1] intValue];
                if ([parts[0] length] && [parts[1] length]) {
                    [ptsX addObject:@(x)];
                    [ptsY addObject:@(y)];
                }
            }
        }
        NSUInteger N = ptsX.count;
        if (N == 0) {
            printf("Largest valid area: 0\n");
            return 0;
        }
        NSArray *ux = [[[NSSet setWithArray:ptsX] allObjects] sortedArrayUsingSelector:@selector(compare:)];
        NSArray *uy = [[[NSSet setWithArray:ptsY] allObjects] sortedArrayUsingSelector:@selector(compare:)];
        NSUInteger W = 2 * ux.count + 1;
        NSUInteger H = 2 * uy.count + 1;
        NSMutableData *colWData = [NSMutableData dataWithLength:W * sizeof(long long)];
        NSMutableData *rowHData = [NSMutableData dataWithLength:H * sizeof(long long)];
        long long *colW = (long long *)[colWData mutableBytes];
        long long *rowH = (long long *)[rowHData mutableBytes];
        colW[0] = 1;
        for (NSUInteger i = 0; i < ux.count; i++) {
            colW[2 * i + 1] = 1;
            if (i + 1 < ux.count) {
                long long gap = [ux[i + 1] intValue] - [ux[i] intValue] - 1;
                if (gap < 0) gap = 0;
                colW[2 * i + 2] = gap;
            } else {
                colW[2 * i + 2] = 1;
            }
        }
        rowH[0] = 1;
        for (NSUInteger i = 0; i < uy.count; i++) {
            rowH[2 * i + 1] = 1;
            if (i + 1 < uy.count) {
                long long gap = [uy[i + 1] intValue] - [uy[i] intValue] - 1;
                if (gap < 0) gap = 0;
                rowH[2 * i + 2] = gap;
            } else {
                rowH[2 * i + 2] = 1;
            }
        }
        NSMutableData *gridData = [NSMutableData dataWithLength:H * W];
        char *grid = (char *)[gridData mutableBytes];
        NSMutableDictionary *xMap = [NSMutableDictionary dictionary];
        NSMutableDictionary *yMap = [NSMutableDictionary dictionary];
        for (NSUInteger i = 0; i < ux.count; i++) xMap[ux[i]] = @(i);
        for (NSUInteger i = 0; i < uy.count; i++) yMap[uy[i]] = @(i);
        for (NSUInteger i = 0; i < N; i++) {
            int ax = [ptsX[i] intValue];
            int ay = [ptsY[i] intValue];
            int bx = [ptsX[(i + 1) % N] intValue];
            int by = [ptsY[(i + 1) % N] intValue];
            NSUInteger gx1 = 2 * [xMap[@(ax)] unsignedIntegerValue] + 1;
            NSUInteger gy1 = 2 * [yMap[@(ay)] unsignedIntegerValue] + 1;
            NSUInteger gx2 = 2 * [xMap[@(bx)] unsignedIntegerValue] + 1;
            NSUInteger gy2 = 2 * [yMap[@(by)] unsignedIntegerValue] + 1;
            if (gx1 == gx2) {
                NSUInteger y0 = (gy1 < gy2) ? gy1 : gy2;
                NSUInteger y1 = (gy1 > gy2) ? gy1 : gy2;
                for (NSUInteger y = y0; y <= y1; y++) if (rowH[y] > 0) grid[y * W + gx1] = 1;
            } else {
                NSUInteger x0 = (gx1 < gx2) ? gx1 : gx2;
                NSUInteger x1 = (gx1 > gx2) ? gx1 : gx2;
                for (NSUInteger x = x0; x <= x1; x++) if (colW[x] > 0) grid[gy1 * W + x] = 1;
            }
        }
        int *qx = malloc(W * H * sizeof(int));
        int *qy = malloc(W * H * sizeof(int));
        int head = 0, tail = 0;
        qx[tail] = 0;
        qy[tail] = 0;
        tail++;
        grid[0] = 2;
        int dirs[4][2] = {{0,1},{0,-1},{1,0},{-1,0}};
        while (head < tail) {
            int cx = qx[head];
            int cy = qy[head];
            head++;
            for (int d = 0; d < 4; d++) {
                int nx = cx + dirs[d][0];
                int ny = cy + dirs[d][1];
                if (nx >= 0 && nx < (int)W && ny >= 0 && ny < (int)H &&
                    grid[ny * W + nx] == 0) {
                    grid[ny * W + nx] = 2;
                    qx[tail] = nx;
                    qy[tail] = ny;
                    tail++;
                }
            }
        }
        free(qx);
        free(qy);
        NSMutableData *PData = [NSMutableData dataWithLength:H * W * sizeof(long long)];
        long long *P = (long long *)[PData mutableBytes];
        for (NSUInteger y = 0; y < H; y++) {
            for (NSUInteger x = 0; x < W; x++) {
                long long v = (grid[y * W + x] != 2) ? colW[x] * rowH[y] : 0;
                long long left = (x > 0) ? P[y * W + (x - 1)] : 0;
                long long up = (y > 0) ? P[(y - 1) * W + x] : 0;
                long long diag = (x > 0 && y > 0) ? P[(y - 1) * W + (x - 1)] : 0;
                P[y * W + x] = v + left + up - diag;
            }
        }
        long long maxArea = 0;
        for (NSUInteger i = 0; i < N; i++) {
            for (NSUInteger j = i; j < N; j++) {
                int ax = [ptsX[i] intValue];
                int ay = [ptsY[i] intValue];
                int bx = [ptsX[j] intValue];
                int by = [ptsY[j] intValue];
                long long w = llabs(ax - bx) + 1;
                long long h = llabs(ay - by) + 1;
                long long area = w * h;
                if (area <= maxArea) continue;
                NSUInteger gx1 = 2 * [xMap[@(ax)] unsignedIntegerValue] + 1;
                NSUInteger gy1 = 2 * [yMap[@(ay)] unsignedIntegerValue] + 1;
                NSUInteger gx2 = 2 * [xMap[@(bx)] unsignedIntegerValue] + 1;
                NSUInteger gy2 = 2 * [yMap[@(by)] unsignedIntegerValue] + 1;
                if (gx1 > gx2) { NSUInteger t = gx1; gx1 = gx2; gx2 = t; }
                if (gy1 > gy2) { NSUInteger t = gy1; gy1 = gy2; gy2 = t; }
                long long total = P[gy2 * W + gx2];
                long long left = (gx1 > 0) ? P[gy2 * W + (gx1 - 1)] : 0;
                long long up = (gy1 > 0) ? P[(gy1 - 1) * W + gx2] : 0;
                long long diag = (gx1 > 0 && gy1 > 0) ? P[(gy1 - 1) * W + (gx1 - 1)] : 0;
                long long valid = total - left - up + diag;
                if (valid == area) maxArea = area;
            }
        }
        printf("Largest valid area: %lld\n", maxArea);
    }
    return 0;
}


#import <Foundation/Foundation.h>

typedef struct { int x, y, z; } Point3D;

static inline Point3D add(Point3D a, Point3D b) {
    return (Point3D){a.x + b.x, a.y + b.y, a.z + b.z};
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableSet<NSString *> *cubeSet = [NSMutableSet set];
        int minX = INT_MAX, minY = INT_MAX, minZ = INT_MAX;
        int maxX = INT_MIN, maxY = INT_MIN, maxZ = INT_MIN;
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSArray<NSString *> *parts = [line componentsSeparatedByString:@","];
            if (parts.count != 3) continue;
            int x = parts[0].intValue, y = parts[1].intValue, z = parts[2].intValue;
            NSString *key = [NSString stringWithFormat:@"%d,%d,%d", x, y, z];
            [cubeSet addObject:key];
            if (x < minX) minX = x;
            if (y < minY) minY = y;
            if (z < minZ) minZ = z;
            if (x > maxX) maxX = x;
            if (y > maxY) maxY = y;
            if (z > maxZ) maxZ = z;
        }
        minX--, minY--, minZ--;
        maxX++, maxY++, maxZ++;
        int dx = maxX - minX + 1;
        int dy = maxY - minY + 1;
        int dz = maxZ - minZ + 1;
        size_t total = (size_t)dx * dy * dz;
        bool *visited = calloc(total, sizeof(bool));
        NSMutableArray *queue = [NSMutableArray array];
        Point3D start = {minX, minY, minZ};
        [queue addObject:[NSValue valueWithBytes:&start objCType:@encode(Point3D)]];
        visited[0] = true;
        Point3D dirs[6] = {{-1,0,0},{1,0,0},{0,-1,0},{0,1,0},{0,0,-1},{0,0,1}};
        int faces = 0;
        while (queue.count) {
            NSValue *val = queue[0];
            [queue removeObjectAtIndex:0];
            Point3D cur;
            [val getValue:&cur];
            for (int i = 0; i < 6; ++i) {
                Point3D nxt = add(cur, dirs[i]);
                if (nxt.x < minX || nxt.y < minY || nxt.z < minZ ||
                    nxt.x > maxX || nxt.y > maxY || nxt.z > maxZ) continue;
                NSString *key = [NSString stringWithFormat:@"%d,%d,%d", nxt.x, nxt.y, nxt.z];
                if ([cubeSet containsObject:key]) {
                    faces++;
                } else {
                    size_t idx = (size_t)(nxt.x - minX) * dy * dz + (nxt.y - minY) * dz + (nxt.z - minZ);
                    if (!visited[idx]) {
                        visited[idx] = true;
                        [queue addObject:[NSValue valueWithBytes:&nxt objCType:@encode(Point3D)]];
                    }
                }
            }
        }
        free(visited);
        printf("%d\n", faces);
    }
    return 0;
}

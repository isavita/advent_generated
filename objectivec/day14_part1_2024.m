
#import <Foundation/Foundation.h>

#define WIDTH 101
#define HEIGHT 103
#define MAX_ROBOTS 1000

typedef struct { int x, y, vx, vy; } Robot;

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSData *data = [NSData dataWithContentsOfFile:@"input.txt"];
        if (!data) return 1;
        NSString *content = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        Robot robots[MAX_ROBOTS];
        int count = 0;
        for (NSString *line in lines) {
            if (count >= MAX_ROBOTS) break;
            int px, py, vx, vy;
            if (sscanf([line UTF8String], "p=%d,%d v=%d,%d", &px, &py, &vx, &vy) == 4) {
                robots[count].x = px;
                robots[count].y = py;
                robots[count].vx = vx;
                robots[count].vy = vy;
                count++;
            }
        }

        for (int step = 0; step < 100; ++step) {
            for (int i = 0; i < count; ++i) {
                robots[i].x = (robots[i].x + robots[i].vx) % WIDTH;
                robots[i].y = (robots[i].y + robots[i].vy) % HEIGHT;
                if (robots[i].x < 0) robots[i].x += WIDTH;
                if (robots[i].y < 0) robots[i].y += HEIGHT;
            }
        }

        long long q1=0,q2=0,q3=0,q4=0;
        for (int i = 0; i < count; ++i) {
            int x = robots[i].x, y = robots[i].y;
            if (x == 50 || y == 51) continue;
            if (x < 50 && y < 51) q1++;
            else if (x > 50 && y < 51) q2++;
            else if (x < 50 && y > 51) q3++;
            else if (x > 50 && y > 51) q4++;
        }

        printf("%lld\n", q1 * q2 * q3 * q4);
    }
    return 0;
}

#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSFileManager *fileManager = [NSFileManager defaultManager];
        NSString *filePath = @"input.txt";
        NSString *input = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        int maxDistance = 0;
        
        for (NSString *line in lines) {
            NSArray *components = [line componentsSeparatedByString:@" "];
            int speed = [components[3] intValue];
            int flyTime = [components[6] intValue];
            int restTime = [components[13] intValue];
            
            int cycleTime = flyTime + restTime;
            int cycles = 2503 / cycleTime;
            int remainingTime = 2503 % cycleTime;
            
            int distance = cycles * flyTime * speed;
            distance += MIN(remainingTime, flyTime) * speed;
            
            if (distance > maxDistance) {
                maxDistance = distance;
            }
        }
        
        printf("%d\n", maxDistance);
    }
    return 0;
}
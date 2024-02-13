
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        int gridSerialNumber = [input intValue];
        
        int maxPower = INT_MIN;
        int maxX = 0;
        int maxY = 0;
        
        for (int x = 1; x <= 300 - 2; x++) {
            for (int y = 1; y <= 300 - 2; y++) {
                int totalPower = 0;
                for (int i = x; i < x + 3; i++) {
                    for (int j = y; j < y + 3; j++) {
                        int rackID = i + 10;
                        int powerLevel = rackID * j;
                        powerLevel += gridSerialNumber;
                        powerLevel *= rackID;
                        powerLevel = (powerLevel / 100) % 10;
                        powerLevel -= 5;
                        totalPower += powerLevel;
                    }
                }
                if (totalPower > maxPower) {
                    maxPower = totalPower;
                    maxX = x;
                    maxY = y;
                }
            }
        }
        
        printf("%d,%d\n", maxX, maxY);
    }
    return 0;
}

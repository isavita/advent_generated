#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *fishAges = [content componentsSeparatedByString:@","];
        
        NSInteger lanternFishCounts[9] = {0};
        for (NSString *age in fishAges) {
            lanternFishCounts[age.integerValue]++;
        }

        for (NSInteger i = 0; i < 256; i++) {
            NSInteger newLanternFish = lanternFishCounts[0];
            for (NSInteger j = 0; j < 8; j++) {
                lanternFishCounts[j] = lanternFishCounts[j + 1];
            }
            lanternFishCounts[6] += newLanternFish;
            lanternFishCounts[8] = newLanternFish;
        }

        NSInteger total = 0;
        for (NSInteger i = 0; i < 9; i++) {
            total += lanternFishCounts[i];
        }
        
        NSLog(@"%ld", (long)total);
    }
    return 0;
}
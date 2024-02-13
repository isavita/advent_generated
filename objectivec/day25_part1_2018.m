#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableArray *points = [NSMutableArray array];
        
        for (NSString *line in lines) {
            if (line.length > 0) {
                NSArray *components = [line componentsSeparatedByString:@","];
                NSMutableArray *point = [NSMutableArray array];
                
                for (NSString *component in components) {
                    [point addObject:@([component intValue])];
                }
                
                [points addObject:point];
            }
        }
        
        NSInteger numConstellations = 0;
        
        for (int i = 0; i < points.count; i++) {
            NSMutableArray *constellation = [NSMutableArray arrayWithObject:points[i]];
            [points removeObjectAtIndex:i];
            i = -1;
            
            for (int j = 0; j < constellation.count; j++) {
                NSMutableArray *currentPoint = constellation[j];
                
                for (int k = 0; k < points.count; k++) {
                    NSMutableArray *nextPoint = points[k];
                    NSInteger distance = 0;
                    
                    for (int l = 0; l < currentPoint.count; l++) {
                        distance += labs([currentPoint[l] integerValue] - [nextPoint[l] integerValue]);
                    }
                    
                    if (distance <= 3) {
                        [constellation addObject:nextPoint];
                        [points removeObjectAtIndex:k];
                        k = -1;
                    }
                }
            }
            
            numConstellations++;
        }
        
        printf("%ld\n", numConstellations);
    }
    return 0;
}
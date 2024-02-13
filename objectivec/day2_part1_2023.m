#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error opening file: %@", error);
            return 1;
        }
        
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"Game (\\d+): (.+)" options:0 error:nil];
        NSRegularExpression *cubeRegex = [NSRegularExpression regularExpressionWithPattern:@"(\\d+) (red|green|blue)" options:0 error:nil];
        NSInteger totalSum = 0;
        
        for (NSString *line in lines) {
            NSTextCheckingResult *match = [regex firstMatchInString:line options:0 range:NSMakeRange(0, [line length])];
            if (match) {
                NSInteger gameId = [[line substringWithRange:[match rangeAtIndex:1]] integerValue];
                NSString *roundsString = [line substringWithRange:[match rangeAtIndex:2]];
                NSArray *rounds = [roundsString componentsSeparatedByString:@";"];
                BOOL isValid = YES;
                
                for (NSString *round in rounds) {
                    NSArray<NSTextCheckingResult *> *cubes = [cubeRegex matchesInString:round options:0 range:NSMakeRange(0, [round length])];
                    NSInteger red = 0, green = 0, blue = 0;
                    
                    for (NSTextCheckingResult *cube in cubes) {
                        NSInteger count = [[round substringWithRange:[cube rangeAtIndex:1]] integerValue];
                        NSString *color = [round substringWithRange:[cube rangeAtIndex:2]];
                        
                        if ([color isEqualToString:@"red"]) {
                            red += count;
                        } else if ([color isEqualToString:@"green"]) {
                            green += count;
                        } else if ([color isEqualToString:@"blue"]) {
                            blue += count;
                        }
                        
                        if (red > 12 || green > 13 || blue > 14) {
                            isValid = NO;
                            break;
                        }
                    }
                    
                    if (!isValid) {
                        break;
                    }
                }
                
                if (isValid) {
                    totalSum += gameId;
                }
            }
        }
        
        printf("%ld\n", totalSum);
    }
    return 0;
}
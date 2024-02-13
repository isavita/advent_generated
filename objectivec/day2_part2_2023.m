#import <Foundation/Foundation.h>

int main() {
    NSError *error;
    NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
    NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"Game (\\d+): (.+)" options:0 error:nil];
    NSRegularExpression *cubeRegex = [NSRegularExpression regularExpressionWithPattern:@"(\\d+) (red|green|blue)" options:0 error:nil];
    int totalPower = 0;

    for (NSString *line in lines) {
        NSTextCheckingResult *matches = [regex firstMatchInString:line options:0 range:NSMakeRange(0, [line length])];

        if (matches.numberOfRanges == 3) {
            NSString *rounds = [[line componentsSeparatedByString:@": "] objectAtIndex:1];
            int maxRed = 0, maxGreen = 0, maxBlue = 0;

            NSArray *roundsArray = [rounds componentsSeparatedByString:@";"];

            for (NSString *round in roundsArray) {
                NSArray *cubes = [cubeRegex matchesInString:round options:0 range:NSMakeRange(0, [round length])];
                int red = 0, green = 0, blue = 0;

                for (NSTextCheckingResult *cube in cubes) {
                    int count = [[round substringWithRange:[cube rangeAtIndex:1]] intValue];
                    NSString *color = [round substringWithRange:[cube rangeAtIndex:2]];

                    if ([color isEqualToString:@"red"]) {
                        red += count;
                    } else if ([color isEqualToString:@"green"]) {
                        green += count;
                    } else if ([color isEqualToString:@"blue"]) {
                        blue += count;
                    }
                }

                if (red > maxRed) {
                    maxRed = red;
                }
                if (green > maxGreen) {
                    maxGreen = green;
                }
                if (blue > maxBlue) {
                    maxBlue = blue;
                }
            }

            int power = maxRed * maxGreen * maxBlue;
            totalPower += power;
        }
    }

    printf("%d\n", totalPower);

    return 0;
}
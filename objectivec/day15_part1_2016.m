#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read input from file
        NSString *filePath = [[NSBundle mainBundle] pathForResource:@"input" ofType:@"txt"];
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];

        // Split the input into lines
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];

        // Parse the disc configurations
        NSMutableArray *discs = [NSMutableArray array];
        for (NSString *line in lines) {
            if ([line length] > 0) {
                NSArray *components = [line componentsSeparatedByString:@" "];
                int discNumber = [[components[1] stringByReplacingOccurrencesOfString:@"#" withString:@""] intValue];
                int positions = [[components[3] stringByReplacingOccurrencesOfString:@"," withString:@""] intValue];
                int initialPosition = [[components[11] stringByReplacingOccurrencesOfString:@"." withString:@""] intValue];

                // Store disc configurations as a dictionary
                NSDictionary *disc = @{@"discNumber": @(discNumber), @"positions": @(positions), @"initialPosition": @(initialPosition)};
                [discs addObject:disc];
            }
        }

        // Find the first time to press the button
        int time = 0;
        BOOL capsuleFallsThrough = NO;

        while (!capsuleFallsThrough) {
            capsuleFallsThrough = YES;
            for (NSDictionary *disc in discs) {
                int discNumber = [[disc objectForKey:@"discNumber"] intValue];
                int positions = [[disc objectForKey:@"positions"] intValue];
                int initialPosition = [[disc objectForKey:@"initialPosition"] intValue];

                // Calculate the position of the disc at the given time
                int discTime = time + discNumber;
                int discPosition = (initialPosition + discTime) % positions;

                // If the capsule does not fall through, break and increment time
                if (discPosition != 0) {
                    capsuleFallsThrough = NO;
                    break;
                }
            }

            if (capsuleFallsThrough) {
                break;
            }

            time++;
        }

        // Print the result
        NSLog(@"First time to press the button: %d", time);
    }
    return 0;
}
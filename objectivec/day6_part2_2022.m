#import <Foundation/Foundation.h>

BOOL allCharactersUnique(NSString *str) {
    NSMutableSet *set = [NSMutableSet setWithCapacity:[str length]];
    for (NSUInteger i = 0; i < [str length]; i++) {
        unichar c = [str characterAtIndex:i];
        if ([set containsObject:@(c)]) {
            return NO;
        }
        [set addObject:@(c)];
    }
    return YES;
}

NSUInteger findMarkerPosition(NSString *data, NSUInteger markerLength) {
    for (NSUInteger i = markerLength - 1; i < [data length]; i++) {
        NSString *substring = [data substringWithRange:NSMakeRange(i - markerLength + 1, markerLength)];
        if (allCharactersUnique(substring)) {
            return i + 1;
        }
    }
    return 0;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error = nil;
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];

        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }

        NSUInteger startOfPacketPosition = findMarkerPosition(fileContent, 4);
        NSUInteger startOfMessagePosition = findMarkerPosition(fileContent, 14);

        NSLog(@"Start-of-packet marker detected after character: %lu", (unsigned long)startOfPacketPosition);
        NSLog(@"Start-of-message marker detected after character: %lu", (unsigned long)startOfMessagePosition);
    }
    return 0;
}
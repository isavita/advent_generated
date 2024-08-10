#import <Foundation/Foundation.h>

const NSInteger diskLength = 35651584;

NSString *readInitialState(NSString *filename) {
    NSError *error = nil;
    NSString *content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:&error];
    if (error) { @throw [NSException exceptionWithName:@"FileReadException" reason:error.localizedDescription userInfo:nil]; }
    return content;
}

NSString *generateData(NSString *initialState, NSInteger length) {
    NSMutableString *data = [NSMutableString stringWithString:initialState];
    while (data.length < length) {
        NSMutableString *b = [NSMutableString stringWithCapacity:data.length];
        for (NSInteger i = data.length - 1; i >= 0; i--) {
            [b appendString:([data characterAtIndex:i] == '0' ? @"1" : @"0")];
        }
        [data appendString:@"0"];
        [data appendString:b];
    }
    return [data substringToIndex:length];
}

NSString *calculateChecksum(NSString *data) {
    while (data.length % 2 == 0) {
        NSMutableString *b = [NSMutableString stringWithCapacity:data.length / 2];
        for (NSInteger i = 0; i < data.length; i += 2) {
            [b appendString:([data characterAtIndex:i] == [data characterAtIndex:i + 1] ? @"1" : @"0")];
        }
        data = b;
    }
    return data;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *initialState = readInitialState(@"input.txt");
        NSString *data = generateData(initialState, diskLength);
        NSString *checksum = calculateChecksum(data);
        NSLog(@"Checksum: %@", checksum);
    }
    return 0;
}
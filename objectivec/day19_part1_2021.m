
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *error = nil;
        NSString *content = [NSString stringWithContentsOfFile:path
                                                      encoding:NSUTF8StringEncoding
                                                         error:&error];
        if (content) {
            printf("%s", [content UTF8String]);
        }
    }
    return 0;
}

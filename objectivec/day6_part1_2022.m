#import <Foundation/Foundation.h>

@interface Solution : NSObject

- (NSString *)readAll:(NSString *)path;
- (NSInteger)firstNUnique:(NSString *)s n:(NSInteger)n;
- (NSSet *)setOf:(NSString *)s;

@end

@implementation Solution

- (NSString *)readAll:(NSString *)path {
    NSError *error;
    return [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&error];
}

- (NSInteger)firstNUnique:(NSString *)s n:(NSInteger)n {
    for (NSInteger i = n; i < s.length; i++) {
        NSString *substr = [s substringWithRange:NSMakeRange(i - n, n)];
        if (substr.length == [[self setOf:substr] count]) {
            return i;
        }
    }
    return -1;
}

- (NSSet *)setOf:(NSString *)s {
    NSMutableSet *set = [NSMutableSet set];
    for (NSInteger i = 0; i < s.length; i++) {
        [set addObject:[s substringWithRange:NSMakeRange(i, 1)]];
    }
    return set;
}

@end

int main() {
    Solution *solution = [[Solution alloc] init];
    NSString *s = [solution readAll:@"input.txt"];
    NSLog(@"%ld", [solution firstNUnique:s n:4]);
    return 0;
}
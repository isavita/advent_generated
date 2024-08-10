#import <Foundation/Foundation.h>

@interface JSONSum : NSObject

- (instancetype)initWithJSONString:(NSString *)jsonString;
- (NSInteger)sumOfNumbers;

@end

@implementation JSONSum {
    NSInteger _sum;
}

- (instancetype)initWithJSONString:(NSString *)jsonString {
    self = [super init];
    if (self) {
        NSError *error = nil;
        id jsonObject = [NSJSONSerialization JSONObjectWithData:[jsonString dataUsingEncoding:NSUTF8StringEncoding]
                                                         options:0
                                                           error:&error];
        if (error) {
            NSLog(@"Error parsing JSON: %@", error);
            return nil;
        }
        [self recursivelySumNumbers:jsonObject];
    }
    return self;
}

- (NSInteger)sumOfNumbers {
    return _sum;
}

- (void)recursivelySumNumbers:(id)jsonObject {
    if ([jsonObject isKindOfClass:[NSNumber class]]) {
        _sum += [jsonObject integerValue];
    } else if ([jsonObject isKindOfClass:[NSDictionary class]]) {
        for (id value in [jsonObject allValues]) {
            [self recursivelySumNumbers:value];
        }
    } else if ([jsonObject isKindOfClass:[NSArray class]]) {
        for (id element in jsonObject) {
            [self recursivelySumNumbers:element];
        }
    }
}

@end

int main(int argc, char *argv[]) {
    NSString *inputPath = @"input.txt";
    NSError *error = nil;
    NSString *inputString = [NSString stringWithContentsOfFile:inputPath
                                                         encoding:NSUTF8StringEncoding
                                                            error:&error];
    if (error) {
        NSLog(@"Error reading input file: %@", error);
        return 1;
    }

    JSONSum *sum = [[JSONSum alloc] initWithJSONString:inputString];
    NSLog(@"Sum of numbers: %ld", (long)sum.sumOfNumbers);

    return 0;
}
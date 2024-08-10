#import <Foundation/Foundation.h>

int sumNumbers(id data);
BOOL containsRed(NSDictionary *obj);

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSData *data = [NSData dataWithContentsOfFile:@"input.txt"];
        id jsonData = [NSJSONSerialization JSONObjectWithData:data options:0 error:nil];
        NSLog(@"%d", sumNumbers(jsonData));
    }
}

int sumNumbers(id data) {
    int sum = 0;
    if ([data isKindOfClass:[NSArray class]]) {
        for (id v in data) {
            sum += sumNumbers(v);
        }
    } else if ([data isKindOfClass:[NSDictionary class]]) {
        NSDictionary *dict = (NSDictionary *)data;
        if (!containsRed(dict)) {
            for (id v in dict.allValues) {
                sum += sumNumbers(v);
            }
        }
    } else if ([data isKindOfClass:[NSNumber class]]) {
        sum += [data intValue];
    }
    return sum;
}

BOOL containsRed(NSDictionary *obj) {
    for (id v in obj.allValues) {
        if ([v isKindOfClass:[NSString class]] && [v isEqualToString:@"red"]) {
            return YES;
        }
    }
    return NO;
}
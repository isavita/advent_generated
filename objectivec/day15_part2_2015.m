#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableArray *ingredients = [NSMutableArray array];
        for (NSString *line in lines) {
            NSArray *components = [line componentsSeparatedByString:@" "];
            NSInteger capacity = [components[2] integerValue];
            NSInteger durability = [components[4] integerValue];
            NSInteger flavor = [components[6] integerValue];
            NSInteger texture = [components[8] integerValue];
            NSInteger calories = [components[10] integerValue];
            NSDictionary *ingredient = @{@"capacity": @(capacity), @"durability": @(durability), @"flavor": @(flavor), @"texture": @(texture), @"calories": @(calories)};
            [ingredients addObject:ingredient];
        }
        
        NSInteger maxScore = 0;
        for (int i = 0; i <= 100; i++) {
            for (int j = 0; j <= 100 - i; j++) {
                for (int k = 0; k <= 100 - i - j; k++) {
                    int l = 100 - i - j - k;
                    
                    NSInteger capacity = MAX(0, i * [ingredients[0][@"capacity"] integerValue] + j * [ingredients[1][@"capacity"] integerValue] + k * [ingredients[2][@"capacity"] integerValue] + l * [ingredients[3][@"capacity"] integerValue]);
                    NSInteger durability = MAX(0, i * [ingredients[0][@"durability"] integerValue] + j * [ingredients[1][@"durability"] integerValue] + k * [ingredients[2][@"durability"] integerValue] + l * [ingredients[3][@"durability"] integerValue]);
                    NSInteger flavor = MAX(0, i * [ingredients[0][@"flavor"] integerValue] + j * [ingredients[1][@"flavor"] integerValue] + k * [ingredients[2][@"flavor"] integerValue] + l * [ingredients[3][@"flavor"] integerValue]);
                    NSInteger texture = MAX(0, i * [ingredients[0][@"texture"] integerValue] + j * [ingredients[1][@"texture"] integerValue] + k * [ingredients[2][@"texture"] integerValue] + l * [ingredients[3][@"texture"] integerValue]);
                    NSInteger calories = i * [ingredients[0][@"calories"] integerValue] + j * [ingredients[1][@"calories"] integerValue] + k * [ingredients[2][@"calories"] integerValue] + l * [ingredients[3][@"calories"] integerValue];
                    
                    if (calories == 500) {
                        NSInteger score = capacity * durability * flavor * texture;
                        if (score > maxScore) {
                            maxScore = score;
                        }
                    }
                }
            }
        }
        
        printf("%ld\n", (long)maxScore);
    }
    return 0;
}
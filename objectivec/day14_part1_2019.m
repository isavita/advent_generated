#import <Foundation/Foundation.h>

@interface Chemical : NSObject

@property NSString *name;
@property int amount;

@end

@implementation Chemical

@end

int calculateOre(NSString *chem, int amount, NSDictionary<NSString *, Chemical *> *reactions, NSDictionary<NSString *, NSArray<Chemical *> *> *ingredients, NSMutableDictionary<NSString *, NSNumber *> *surplus) {
    if ([chem isEqualToString:@"ORE"]) {
        return amount;
    }

    if ([surplus[chem] intValue] >= amount) {
        surplus[chem] = @([surplus[chem] intValue] - amount);
        return 0;
    }

    amount -= [surplus[chem] intValue];
    surplus[chem] = @0;
    Chemical *reaction = reactions[chem];
    int times = (amount + reaction.amount - 1) / reaction.amount;
    int ore = 0;

    for (Chemical *ingredient in ingredients[chem]) {
        ore += calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus);
    }

    surplus[chem] = @(times * reaction.amount - amount);
    return ore;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];

        if (error) {
            NSLog(@"Error opening file: %@", error);
            return 1;
        }

        NSMutableDictionary<NSString *, Chemical *> *reactions = [NSMutableDictionary dictionary];
        NSMutableDictionary<NSString *, NSArray<Chemical *> *> *ingredients = [NSMutableDictionary dictionary];
        NSMutableDictionary<NSString *, NSNumber *> *surplus = [NSMutableDictionary dictionary];

        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        for (NSString *line in lines) {
            NSArray *parts = [line componentsSeparatedByString:@" => "];
            Chemical *output = [[Chemical alloc] init];
            output.amount = [parts[1] intValue];
            output.name = [parts[1] componentsSeparatedByString:@" "][1];

            NSMutableArray<Chemical *> *inputs = [NSMutableArray array];
            for (NSString *in in [parts[0] componentsSeparatedByString:@", "]) {
                Chemical *input = [[Chemical alloc] init];
                input.amount = [[in componentsSeparatedByString:@" "][0] intValue];
                input.name = [in componentsSeparatedByString:@" "][1];
                [inputs addObject:input];
            }

            reactions[output.name] = output;
            ingredients[output.name] = inputs;
        }

        printf("%d\n", calculateOre(@"FUEL", 1, reactions, ingredients, surplus));
    }
    return 0;
}
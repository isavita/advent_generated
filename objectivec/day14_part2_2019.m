#import <Foundation/Foundation.h>

typedef struct {
    NSMutableDictionary *inputs;
    NSString *output;
    NSInteger outputQuantity;
} Reaction;

NSMutableDictionary *parseReactions(NSString *filePath) {
    NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    NSArray *lines = [content componentsSeparatedByString:@"\n"];
    NSMutableDictionary *reactions = [NSMutableDictionary dictionary];

    for (NSString *line in lines) {
        if ([line length] > 0) {
            NSArray *parts = [line componentsSeparatedByString:@" => "];
            NSArray *inputs = [[parts[0] componentsSeparatedByString:@", "] mutableCopy];
            NSArray *outputParts = [[parts[1] componentsSeparatedByString:@" "] mutableCopy];

            NSString *outputChemical = outputParts[1];
            NSInteger outputQuantity = [outputParts[0] integerValue];

            NSMutableDictionary *inputDict = [NSMutableDictionary dictionary];
            for (NSString *input in inputs) {
                NSArray *inputParts = [input componentsSeparatedByString:@" "];
                NSString *inputChemical = inputParts[1];
                NSInteger inputQuantity = [inputParts[0] integerValue];
                inputDict[inputChemical] = @(inputQuantity);
            }

            Reaction reaction = { inputDict, outputChemical, outputQuantity };
            reactions[outputChemical] = [NSValue valueWithBytes:&reaction objCType:@encode(Reaction)];
        }
    }

    return reactions;
}

NSInteger calculateOreRequired(NSMutableDictionary *reactions, NSString *targetChemical, NSInteger targetQuantity, NSMutableDictionary *surplus) {
    if ([targetChemical isEqualToString:@"ORE"]) {
        return targetQuantity;
    }

    NSInteger required = targetQuantity;
    if (surplus[targetChemical]) {
        NSInteger available = [surplus[targetChemical] integerValue];
        if (available >= required) {
            surplus[targetChemical] = @(available - required);
            return 0;
        } else {
            required -= available;
            surplus[targetChemical] = @0;
        }
    }

    Reaction reaction;
    [reactions[targetChemical] getValue:&reaction];
    NSInteger multiplier = (required + reaction.outputQuantity - 1) / reaction.outputQuantity;

    NSInteger oreRequired = 0;
    for (NSString *inputChemical in reaction.inputs) {
        NSInteger inputQuantity = [reaction.inputs[inputChemical] integerValue];
        oreRequired += calculateOreRequired(reactions, inputChemical, inputQuantity * multiplier, surplus);
    }

    NSInteger produced = reaction.outputQuantity * multiplier;
    surplus[targetChemical] = @(surplus[targetChemical] ? [surplus[targetChemical] integerValue] + produced - required : produced - required);

    return oreRequired;
}

NSInteger findMaxFuel(NSMutableDictionary *reactions, NSInteger oreLimit) {
    NSInteger low = 0;
    NSInteger high = 1;

    while (calculateOreRequired(reactions, @"FUEL", high, [NSMutableDictionary dictionary]) <= oreLimit) {
        low = high;
        high *= 2;
    }

    while (low < high - 1) {
        NSInteger mid = (low + high) / 2;
        if (calculateOreRequired(reactions, @"FUEL", mid, [NSMutableDictionary dictionary]) <= oreLimit) {
            low = mid;
        } else {
            high = mid;
        }
    }

    return low;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSMutableDictionary *reactions = parseReactions(filePath);

        NSInteger oreRequired = calculateOreRequired(reactions, @"FUEL", 1, [NSMutableDictionary dictionary]);
        NSLog(@"Minimum ORE required to produce 1 FUEL: %ld", (long)oreRequired);

        NSInteger maxFuel = findMaxFuel(reactions, 1000000000000);
        NSLog(@"Maximum FUEL that can be produced with 1 trillion ORE: %ld", (long)maxFuel);
    }
    return 0;
}
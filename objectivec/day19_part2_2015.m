#import <Foundation/Foundation.h>

@interface MedicineCalibrator : NSObject
- (NSInteger)calibrateWithInput:(NSString *)input;
- (NSInteger)fabricateWithInput:(NSString *)input;
@end

@implementation MedicineCalibrator {
    NSMutableDictionary *replacements;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        replacements = [NSMutableDictionary dictionary];
    }
    return self;
}

- (void)parseReplacements:(NSArray *)lines {
    for (NSString *line in lines) {
        NSArray *components = [line componentsSeparatedByString:@" => "];
        if (components.count == 2) {
            NSString *key = components[0];
            NSString *value = components[1];
            if (!replacements[key]) {
                replacements[key] = [NSMutableArray array];
            }
            [replacements[key] addObject:value];
        }
    }
}

- (NSInteger)calibrateWithInput:(NSString *)input {
    NSArray *lines = [input componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
    NSString *medicineMolecule = lines.lastObject;
    [self parseReplacements:[lines subarrayWithRange:NSMakeRange(0, lines.count - 2)]];

    NSMutableSet *distinctMolecules = [NSMutableSet set];
    for (NSString *key in replacements) {
        NSRange searchRange = NSMakeRange(0, medicineMolecule.length);
        NSRange range = [medicineMolecule rangeOfString:key options:0 range:searchRange];
        while (range.location != NSNotFound) {
            for (NSString *replacement in replacements[key]) {
                NSString *newMolecule = [medicineMolecule stringByReplacingCharactersInRange:range withString:replacement];
                [distinctMolecules addObject:newMolecule];
            }
            searchRange = NSMakeRange(range.location + range.length, medicineMolecule.length - (range.location + range.length));
            range = [medicineMolecule rangeOfString:key options:0 range:searchRange];
        }
    }
    return distinctMolecules.count;
}

- (NSInteger)fabricateWithInput:(NSString *)input {
    NSArray *lines = [input componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
    NSString *medicineMolecule = lines.lastObject;
    [self parseReplacements:[lines subarrayWithRange:NSMakeRange(0, lines.count - 2)]];

    NSMutableDictionary *reverseReplacements = [NSMutableDictionary dictionary];
    for (NSString *key in replacements) {
        for (NSString *value in replacements[key]) {
            reverseReplacements[value] = key;
        }
    }

    NSInteger steps = 0;
    NSString *currentMolecule = medicineMolecule;
    while (![currentMolecule isEqualToString:@"e"]) {
        for (NSString *key in reverseReplacements) {
            NSRange range = [currentMolecule rangeOfString:key];
            if (range.location != NSNotFound) {
                currentMolecule = [currentMolecule stringByReplacingCharactersInRange:range withString:reverseReplacements[key]];
                steps++;
                break;
            }
        }
    }
    return steps;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error.localizedDescription);
            return 1;
        }

        MedicineCalibrator *calibrator = [[MedicineCalibrator alloc] init];
        NSInteger distinctMolecules = [calibrator calibrateWithInput:fileContents];
        NSInteger fabricationSteps = [calibrator fabricateWithInput:fileContents];

        NSLog(@"Number of distinct molecules: %ld", (long)distinctMolecules);
        NSLog(@"Fewest number of steps to fabricate the medicine: %ld", (long)fabricationSteps);
    }
    return 0;
}
#import <Foundation/Foundation.h>

NSArray<NSString *> *readInputFromFile(NSString *filename) {
    NSString *filePath = [[NSBundle mainBundle] pathForResource:filename ofType:nil];
    NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    NSArray<NSString *> *lines = [content componentsSeparatedByString:@"\n"];
    return lines;
}

NSString *calculateGammaRate(NSArray<NSString *> *diagnosticReport) {
    NSUInteger bitLength = [diagnosticReport[0] length];
    NSMutableArray<NSMutableArray *> *bitCounts = [NSMutableArray arrayWithCapacity:bitLength];
    for (NSUInteger i = 0; i < bitLength; i++) {
        bitCounts[i] = [NSMutableArray arrayWithObjects:@0, @0, nil];
    }

    for (NSString *binaryNumber in diagnosticReport) {
        for (NSUInteger i = 0; i < bitLength; i++) {
            unichar bit = [binaryNumber characterAtIndex:i];
            NSInteger bitValue = bit - '0';
            bitCounts[i][bitValue] = @([bitCounts[i][bitValue] integerValue] + 1);
        }
    }

    NSMutableString *gammaRate = [NSMutableString stringWithCapacity:bitLength];
    for (NSMutableArray *count in bitCounts) {
        NSInteger zeroCount = [count[0] integerValue];
        NSInteger oneCount = [count[1] integerValue];
        [gammaRate appendString:(oneCount >= zeroCount ? @"1" : @"0")];
    }

    return gammaRate;
}

NSString *calculateEpsilonRate(NSString *gammaRate) {
    NSMutableString *epsilonRate = [NSMutableString stringWithCapacity:[gammaRate length]];
    for (NSUInteger i = 0; i < [gammaRate length]; i++) {
        unichar bit = [gammaRate characterAtIndex:i];
        [epsilonRate appendString:(bit == '1' ? @"0" : @"1")];
    }
    return epsilonRate;
}

NSInteger binaryStringToDecimal(NSString *binaryString) {
    return (NSInteger)strtoul([binaryString UTF8String], NULL, 2);
}

NSString *filterNumbersByBitCriteria(NSArray<NSString *> *numbers, BOOL mostCommon, NSUInteger bitIndex) {
    NSMutableArray<NSString *> *filteredNumbers = [NSMutableArray arrayWithArray:numbers];
    NSUInteger bitLength = [numbers[0] length];

    for (NSUInteger i = bitIndex; i < bitLength && [filteredNumbers count] > 1; i++) {
        NSMutableArray<NSString *> *ones = [NSMutableArray array];
        NSMutableArray<NSString *> *zeros = [NSMutableArray array];

        for (NSString *number in filteredNumbers) {
            unichar bit = [number characterAtIndex:i];
            if (bit == '1') {
                [ones addObject:number];
            } else {
                [zeros addObject:number];
            }
        }

        if (mostCommon) {
            filteredNumbers = [ones count] >= [zeros count] ? ones : zeros;
        } else {
            filteredNumbers = [zeros count] <= [ones count] ? zeros : ones;
        }
    }

    return filteredNumbers[0];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray<NSString *> *diagnosticReport = readInputFromFile(@"input.txt");

        // Part 1: Power Consumption
        NSString *gammaRate = calculateGammaRate(diagnosticReport);
        NSString *epsilonRate = calculateEpsilonRate(gammaRate);
        NSInteger gammaRateDecimal = binaryStringToDecimal(gammaRate);
        NSInteger epsilonRateDecimal = binaryStringToDecimal(epsilonRate);
        NSInteger powerConsumption = gammaRateDecimal * epsilonRateDecimal;
        NSLog(@"Power Consumption: %ld", (long)powerConsumption);

        // Part 2: Life Support Rating
        NSString *oxygenGeneratorRating = filterNumbersByBitCriteria(diagnosticReport, YES, 0);
        NSString *co2ScrubberRating = filterNumbersByBitCriteria(diagnosticReport, NO, 0);
        NSInteger oxygenGeneratorRatingDecimal = binaryStringToDecimal(oxygenGeneratorRating);
        NSInteger co2ScrubberRatingDecimal = binaryStringToDecimal(co2ScrubberRating);
        NSInteger lifeSupportRating = oxygenGeneratorRatingDecimal * co2ScrubberRatingDecimal;
        NSLog(@"Life Support Rating: %ld", (long)lifeSupportRating);
    }
    return 0;
}
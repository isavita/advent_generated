#import <Foundation/Foundation.h>

// Function to read weights from input.txt
NSArray<NSNumber *> *readWeightsFromFile(NSString *filename) {
    NSString *filePath = [[NSBundle mainBundle] pathForResource:@"input" ofType:@"txt"];
    NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    NSArray *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
    NSMutableArray<NSNumber *> *weights = [NSMutableArray array];
    for (NSString *line in lines) {
        if (line.length > 0) {
            [weights addObject:@([line integerValue])];
        }
    }
    return weights;
}

// Function to calculate quantum entanglement
NSUInteger quantumEntanglement(NSArray<NSNumber *> *group) {
    NSUInteger qe = 1;
    for (NSNumber *weight in group) {
        qe *= [weight unsignedIntegerValue];
    }
    return qe;
}

// Helper function to find the optimal configuration using recursive backtracking
void findOptimalConfigurationHelper(NSArray<NSNumber *> *weights, NSUInteger targetWeight, NSUInteger currentIndex, NSUInteger currentSum, NSUInteger currentCount, NSUInteger *minPackages, NSUInteger *minQuantumEntanglement, NSMutableArray<NSNumber *> *currentGroup) {
    if (currentSum == targetWeight) {
        if (currentCount < *minPackages) {
            *minPackages = currentCount;
            *minQuantumEntanglement = quantumEntanglement(currentGroup);
        } else if (currentCount == *minPackages) {
            NSUInteger currentQE = quantumEntanglement(currentGroup);
            if (currentQE < *minQuantumEntanglement) {
                *minQuantumEntanglement = currentQE;
            }
        }
        return;
    }

    if (currentSum > targetWeight || currentIndex >= weights.count) {
        return;
    }

    // Include the current weight
    [currentGroup addObject:weights[currentIndex]];
    findOptimalConfigurationHelper(weights, targetWeight, currentIndex + 1, currentSum + [weights[currentIndex] unsignedIntegerValue], currentCount + 1, minPackages, minQuantumEntanglement, currentGroup);
    [currentGroup removeLastObject];

    // Exclude the current weight
    findOptimalConfigurationHelper(weights, targetWeight, currentIndex + 1, currentSum, currentCount, minPackages, minQuantumEntanglement, currentGroup);
}

// Function to find the optimal configuration
void findOptimalConfiguration(NSArray<NSNumber *> *weights, NSUInteger numGroups) {
    NSUInteger totalWeight = 0;
    for (NSNumber *weight in weights) {
        totalWeight += [weight unsignedIntegerValue];
    }

    if (totalWeight % numGroups != 0) {
        NSLog(@"Impossible to split into %lu equal groups", (unsigned long)numGroups);
        return;
    }

    NSUInteger targetWeight = totalWeight / numGroups;
    NSUInteger minPackages = NSUIntegerMax;
    NSUInteger minQuantumEntanglement = NSUIntegerMax;
    NSMutableArray<NSNumber *> *currentGroup = [NSMutableArray array];

    findOptimalConfigurationHelper(weights, targetWeight, 0, 0, 0, &minPackages, &minQuantumEntanglement, currentGroup);

    NSLog(@"Quantum Entanglement for %lu groups: %lu", (unsigned long)numGroups, (unsigned long)minQuantumEntanglement);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray<NSNumber *> *weights = readWeightsFromFile(@"input.txt");

        // Part One: Three groups
        findOptimalConfiguration(weights, 3);

        // Part Two: Four groups
        findOptimalConfiguration(weights, 4);
    }
    return 0;
}
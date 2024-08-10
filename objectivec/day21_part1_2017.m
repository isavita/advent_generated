#import <Foundation/Foundation.h>

NSString *rotate(NSString *input) {
    NSArray *parts = [input componentsSeparatedByString:@"/"];
    NSMutableArray *newParts = [NSMutableArray arrayWithCapacity:parts.count];
    for (NSInteger x = 0; x < parts.count; x++) {
        NSMutableString *newRow = [NSMutableString string];
        for (NSInteger y = parts.count - 1; y >= 0; y--) {
            [newRow appendFormat:@"%C", [parts[y] characterAtIndex:x]];
        }
        [newParts addObject:newRow];
    }
    return [newParts componentsJoinedByString:@"/"];
}

NSString *flip(NSString *input) {
    NSArray *parts = [input componentsSeparatedByString:@"/"];
    NSMutableArray *flippedParts = [NSMutableArray arrayWithCapacity:parts.count];
    for (NSString *part in parts) {
        NSMutableString *reversed = [NSMutableString stringWithCapacity:part.length];
        for (NSInteger i = part.length - 1; i >= 0; i--) {
            [reversed appendFormat:@"%C", [part characterAtIndex:i]];
        }
        [flippedParts addObject:reversed];
    }
    return [flippedParts componentsJoinedByString:@"/"];
}

NSString *enhance(NSString *input, NSDictionary *rules) {
    for (NSInteger i = 0; i < 4; i++) {
        if (rules[input]) return rules[input];
        input = rotate(input);
    }
    input = flip(input);
    for (NSInteger i = 0; i < 4; i++) {
        if (rules[input]) return rules[input];
        input = rotate(input);
    }
    return @"";
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSMutableDictionary *rules = [NSMutableDictionary dictionary];
        NSString *fileContent = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        for (NSString *line in [fileContent componentsSeparatedByString:@"\n"]) {
            NSArray *parts = [line componentsSeparatedByString:@" => "];
            if (parts.count == 2) {
                rules[parts[0]] = parts[1];
            }
        }

        NSMutableArray *grid = [NSMutableArray arrayWithObjects:@".#.", @"..#", @"###", nil];

        for (NSInteger i = 0; i < 5; i++) {
            NSInteger newSize, subSize;
            if (grid.count % 2 == 0) {
                subSize = 2;
                newSize = grid.count / 2 * 3;
            } else {
                subSize = 3;
                newSize = grid.count / 3 * 4;
            }

            NSMutableArray *newGrid = [NSMutableArray arrayWithCapacity:newSize];
            for (NSInteger x = 0; x < newSize; x++) {
                [newGrid addObject:@""];
            }

            for (NSInteger y = 0; y < grid.count; y += subSize) {
                for (NSInteger x = 0; x < grid.count; x += subSize) {
                    NSMutableArray *square = [NSMutableArray array];
                    for (NSInteger dy = 0; dy < subSize; dy++) {
                        [square addObject:[grid[y + dy] substringWithRange:NSMakeRange(x, subSize)]];
                    }
                    NSString *newSquare = enhance([square componentsJoinedByString:@"/"], rules);
                    NSArray *newRows = [newSquare componentsSeparatedByString:@"/"];
                    for (NSInteger dy = 0; dy < newRows.count; dy++) {
                        newGrid[y / subSize * (subSize + 1) + dy] = [newGrid[y / subSize * (subSize + 1) + dy] stringByAppendingString:newRows[dy]];
                    }
                }
            }
            grid = newGrid;
        }

        NSInteger count = 0;
        for (NSString *row in grid) {
            for (NSUInteger j = 0; j < row.length; j++) {
                if ([row characterAtIndex:j] == '#') count++;
            }
        }
        NSLog(@"%ld", (long)count);
    }
    return 0;
}
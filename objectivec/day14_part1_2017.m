#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonDigest.h>

// Function to perform a single round of the knot hash
void knotHashRound(uint8_t *list, int listLength, NSArray<NSNumber *> *lengths, int *currentPosition, int *skipSize) {
    for (NSNumber *length in lengths) {
        int len = [length intValue];
        uint8_t temp[len];
        for (int i = 0; i < len; i++) {
            temp[i] = list[(*currentPosition + i) % listLength];
        }
        for (int i = 0; i < len; i++) {
            list[(*currentPosition + i) % listLength] = temp[len - 1 - i];
        }
        *currentPosition = (*currentPosition + len + *skipSize) % listLength;
        (*skipSize)++;
    }
}

// Function to generate the dense hash
NSString *denseHash(uint8_t *sparseHash, int length) {
    NSMutableString *denseHash = [NSMutableString string];
    for (int i = 0; i < 16; i++) {
        uint8_t xorResult = sparseHash[i * 16];
        for (int j = 1; j < 16; j++) {
            xorResult ^= sparseHash[i * 16 + j];
        }
        [denseHash appendFormat:@"%02x", xorResult];
    }
    return denseHash;
}

// Function to generate the knot hash
NSString *knotHash(NSString *input) {
    uint8_t list[256];
    for (int i = 0; i < 256; i++) {
        list[i] = (uint8_t)i;
    }
    NSMutableArray<NSNumber *> *lengths = [NSMutableArray array];
    for (int i = 0; i < [input length]; i++) {
        [lengths addObject:@([input characterAtIndex:i])];
    }
    [lengths addObjectsFromArray:@[@17, @31, @73, @47, @23]];

    int currentPosition = 0;
    int skipSize = 0;
    for (int i = 0; i < 64; i++) {
        knotHashRound(list, 256, lengths, &currentPosition, &skipSize);
    }

    return denseHash(list, 256);
}

// Function to convert hex to binary string
NSString *hexToBinary(NSString *hex) {
    NSMutableString *binary = [NSMutableString string];
    for (int i = 0; i < [hex length]; i++) {
        unichar c = [hex characterAtIndex:i];
        switch (c) {
            case '0': [binary appendString:@"0000"]; break;
            case '1': [binary appendString:@"0001"]; break;
            case '2': [binary appendString:@"0010"]; break;
            case '3': [binary appendString:@"0011"]; break;
            case '4': [binary appendString:@"0100"]; break;
            case '5': [binary appendString:@"0101"]; break;
            case '6': [binary appendString:@"0110"]; break;
            case '7': [binary appendString:@"0111"]; break;
            case '8': [binary appendString:@"1000"]; break;
            case '9': [binary appendString:@"1001"]; break;
            case 'a': case 'A': [binary appendString:@"1010"]; break;
            case 'b': case 'B': [binary appendString:@"1011"]; break;
            case 'c': case 'C': [binary appendString:@"1100"]; break;
            case 'd': case 'D': [binary appendString:@"1101"]; break;
            case 'e': case 'E': [binary appendString:@"1110"]; break;
            case 'f': case 'F': [binary appendString:@"1111"]; break;
            default: break;
        }
    }
    return binary;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read input from file
        NSString *filePath = @"input.txt";
        NSString *input = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        input = [input stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];

        int usedSquares = 0;

        // Generate the grid and count used squares
        for (int row = 0; row < 128; row++) {
            NSString *rowInput = [NSString stringWithFormat:@"%@-%d", input, row];
            NSString *knotHashResult = knotHash(rowInput);
            NSString *binaryString = hexToBinary(knotHashResult);
            for (int i = 0; i < [binaryString length]; i++) {
                if ([binaryString characterAtIndex:i] == '1') {
                    usedSquares++;
                }
            }
        }

        // Output the result
        NSLog(@"Used squares: %d", usedSquares);
    }
    return 0;
}
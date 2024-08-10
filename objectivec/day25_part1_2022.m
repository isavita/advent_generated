#import <Foundation/Foundation.h>

// Function to convert a SNAFU number to a decimal number
long long snafuToDecimal(NSString *snafu) {
    long long decimal = 0;
    long long placeValue = 1;

    for (NSInteger i = [snafu length] - 1; i >= 0; i--) {
        unichar c = [snafu characterAtIndex:i];
        long long value = 0;

        switch (c) {
            case '=':
                value = -2;
                break;
            case '-':
                value = -1;
                break;
            case '0':
                value = 0;
                break;
            case '1':
                value = 1;
                break;
            case '2':
                value = 2;
                break;
            default:
                break;
        }

        decimal += value * placeValue;
        placeValue *= 5;
    }

    return decimal;
}

// Function to convert a decimal number to a SNAFU number
NSString *decimalToSnafu(long long decimal) {
    NSMutableString *snafu = [NSMutableString string];

    while (decimal > 0) {
        long long remainder = decimal % 5;
        decimal /= 5;

        if (remainder <= 2) {
            [snafu insertString:[NSString stringWithFormat:@"%lld", remainder] atIndex:0];
        } else {
            [snafu insertString:(remainder == 3) ? @"=" : @"-" atIndex:0];
            decimal += 1;
        }
    }

    return snafu;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read input from file
        NSString *filePath = @"input.txt";
        NSError *error;
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];

        if (!fileContents) {
            NSLog(@"Error reading file: %@", error.localizedDescription);
            return 1;
        }

        // Split the file contents into lines
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];

        // Sum up the decimal values of the SNAFU numbers
        long long sum = 0;
        for (NSString *line in lines) {
            if ([line length] > 0) {
                sum += snafuToDecimal(line);
            }
        }

        // Convert the sum back to a SNAFU number
        NSString *resultSnafu = decimalToSnafu(sum);

        // Print the resulting SNAFU number
        NSLog(@"%@", resultSnafu);
    }
    return 0;
}
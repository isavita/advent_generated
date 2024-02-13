#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *passports = [input componentsSeparatedByString:@"\n\n"];
        
        int validPassports = 0;
        NSArray *requiredFields = @[@"byr", @"iyr", @"eyr", @"hgt", @"hcl", @"ecl", @"pid"];
        
        for (NSString *passport in passports) {
            BOOL isValid = YES;
            for (NSString *field in requiredFields) {
                if (![passport containsString:field]) {
                    isValid = NO;
                    break;
                }
            }
            if (isValid) {
                validPassports++;
            }
        }
        
        printf("%d\n", validPassports);
    }
    return 0;
}
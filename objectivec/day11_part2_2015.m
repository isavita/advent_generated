#import <Foundation/Foundation.h>

NSString *readInput(NSString *filename) {
    NSError *error = nil;
    NSString *content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:&error];
    return error ? nil : content;
}

NSString *incrementPassword(NSString *password) {
    NSMutableString *newPassword = [password mutableCopy];
    for (NSInteger i = newPassword.length - 1; i >= 0; i--) {
        unichar c = [newPassword characterAtIndex:i] + 1;
        if (c > 'z') {
            c = 'a';
        }
        [newPassword replaceCharactersInRange:NSMakeRange(i, 1) withString:[NSString stringWithFormat:@"%C", c]];
        if (c != 'a') break;
    }
    return newPassword;
}

BOOL hasStraight(NSString *password) {
    for (NSInteger i = 0; i < password.length - 2; i++) {
        if ([password characterAtIndex:i] + 1 == [password characterAtIndex:i + 1] &&
            [password characterAtIndex:i] + 2 == [password characterAtIndex:i + 2]) {
            return YES;
        }
    }
    return NO;
}

BOOL containsInvalidLetters(NSString *password) {
    NSCharacterSet *invalidSet = [NSCharacterSet characterSetWithCharactersInString:@"iol"];
    return [password rangeOfCharacterFromSet:invalidSet].location != NSNotFound;
}

BOOL hasTwoPairs(NSString *password) {
    NSInteger count = 0;
    for (NSInteger i = 0; i < password.length - 1; i++) {
        if ([password characterAtIndex:i] == [password characterAtIndex:i + 1]) {
            count++;
            i++; // Skip the next character
        }
    }
    return count >= 2;
}

BOOL isValidPassword(NSString *password) {
    return hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password);
}

NSString *findNextPassword(NSString *password) {
    while (YES) {
        password = incrementPassword(password);
        if (isValidPassword(password)) {
            break;
        }
    }
    return password;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *currentPassword = readInput(@"input.txt");
        NSString *firstNewPassword = findNextPassword(currentPassword);
        NSString *secondNewPassword = findNextPassword(firstNewPassword);
        NSLog(@"%@", secondNewPassword);
    }
    return 0;
}
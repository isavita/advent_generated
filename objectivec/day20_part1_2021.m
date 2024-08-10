#import <Foundation/Foundation.h>

#define ALGORITHM_LENGTH 512

NSString *readFile(NSString *filename) {
    NSString *filePath = [[NSBundle mainBundle] pathForResource:@"input" ofType:@"txt"];
    NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    return fileContents;
}

NSArray<NSString *> *parseInput(NSString *input) {
    NSArray<NSString *> *lines = [input componentsSeparatedByString:@"\n"];
    NSMutableArray<NSString *> *algorithm = [NSMutableArray array];
    NSMutableArray<NSString *> *image = [NSMutableArray array];
    BOOL isAlgorithm = YES;

    for (NSString *line in lines) {
        if ([line isEqualToString:@""]) {
            isAlgorithm = NO;
            continue;
        }
        if (isAlgorithm) {
            [algorithm addObject:line];
        } else {
            [image addObject:line];
        }
    }

    return @[algorithm, image];
}

int getIndexForPixel(NSArray<NSString *> *image, int x, int y, char defaultPixel) {
    int index = 0;
    for (int j = -1; j <= 1; j++) {
        for (int i = -1; i <= 1; i++) {
            index <<= 1;
            if (x + i >= 0 && x + i < image[0].length && y + j >= 0 && y + j < image.count) {
                char pixel = [image[y + j] characterAtIndex:x + i];
                index |= (pixel == '#') ? 1 : 0;
            } else {
                index |= (defaultPixel == '#') ? 1 : 0;
            }
        }
    }
    return index;
}

NSArray<NSString *> *enhanceImage(NSArray<NSString *> *image, NSString *algorithm, char defaultPixel) {
    NSMutableArray<NSString *> *newImage = [NSMutableArray array];
    int width = image[0].length;
    int height = image.count;

    for (int y = -1; y <= height; y++) {
        NSMutableString *row = [NSMutableString string];
        for (int x = -1; x <= width; x++) {
            int index = getIndexForPixel(image, x, y, defaultPixel);
            char newPixel = [algorithm characterAtIndex:index];
            [row appendFormat:@"%c", newPixel];
        }
        [newImage addObject:row];
    }

    return newImage;
}

int countLitPixels(NSArray<NSString *> *image) {
    int count = 0;
    for (NSString *row in image) {
        for (int i = 0; i < row.length; i++) {
            if ([row characterAtIndex:i] == '#') {
                count++;
            }
        }
    }
    return count;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = readFile(@"input.txt");
        NSArray<NSString *> *parsedInput = parseInput(input);
        NSArray<NSString *> *algorithmParts = parsedInput[0];
        NSArray<NSString *> *image = parsedInput[1];

        NSString *algorithm = [algorithmParts componentsJoinedByString:@""];

        char defaultPixel = '.';
        image = enhanceImage(image, algorithm, defaultPixel);
        defaultPixel = ([algorithm characterAtIndex:0] == '#') ? '#' : '.';
        image = enhanceImage(image, algorithm, defaultPixel);

        int litPixels = countLitPixels(image);
        printf("Number of lit pixels: %d\n", litPixels);
    }
    return 0;
}
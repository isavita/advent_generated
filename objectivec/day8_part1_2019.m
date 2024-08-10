#import <Foundation/Foundation.h>

int main() {
    NSError *error;
    NSString *imageData = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
    
    // Check if file exists or there was an error reading it
    if (!imageData) {
        if (error) {
            NSLog(@"Error reading file: %@", error);
        } else {
            NSLog(@"File 'input.txt' not found or couldn't be read.");
        }
        return 1;
    }

    imageData = [imageData stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceCharacterSet];

    // Check if image data is not empty
    if (imageData.length == 0) {
        NSLog(@"Input data is empty.");
        return 1;
    }

    int width = 25, height = 6;
    int layerSize = width * height;

    int minZeros = layerSize + 1;
    int result = 0;

    // Check if image data length is a multiple of layer size
    if (imageData.length % layerSize != 0) {
        NSLog(@"Invalid input data length.");
        return 1;
    }

    for (int i = 0; i < imageData.length; i += layerSize) {
        NSRange layerRange = NSMakeRange(i, MIN(layerSize, (int)imageData.length - i));
        NSString *layer = [imageData substringWithRange:layerRange];
        int zeroCount = 0, oneCount = 0, twoCount = 0;

        for (int j = 0; j < layer.length; j++) {
            switch ([layer characterAtIndex:j]) {
                case '0':
                    zeroCount++;
                    break;
                case '1':
                    oneCount++;
                    break;
                case '2':
                    twoCount++;
                    break;
                default:
                    NSLog(@"Invalid character in input data: %c", [layer characterAtIndex:j]);
                    return 1;
            }
        }

        if (zeroCount < minZeros) {
            minZeros = zeroCount;
            result = oneCount * twoCount;
        }
    }

    NSLog(@"%d", result);

    return 0;
}
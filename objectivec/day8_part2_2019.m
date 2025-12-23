
#import <Foundation/Foundation.h>

int main(int argc, char *argv[]) {
    @autoreleasepool {
        NSData *data = [NSData dataWithContentsOfFile:@"input.txt"];
        if (!data) { fprintf(stderr, "Error reading input file\n"); return 1; }

        const unsigned char *bytes = data.bytes;
        size_t fileSize = data.length;
        const size_t width = 25, height = 6;
        size_t layerSize = width * height;

        char *finalImage = malloc(layerSize);
        if (!finalImage) { fprintf(stderr, "Memory allocation failed\n"); return 1; }
        for (size_t i = 0; i < layerSize; ++i) finalImage[i] = '2';

        for (size_t i = 0; i < fileSize; i += layerSize) {
            size_t layerEnd = (i + layerSize < fileSize) ? i + layerSize : fileSize;
            for (size_t j = 0; j < layerEnd - i; ++j) {
                if (finalImage[j] == '2') finalImage[j] = bytes[i + j];
            }
        }

        for (size_t y = 0; y < height; ++y) {
            for (size_t x = 0; x < width; ++x) {
                char pixel = finalImage[y * width + x];
                putchar(pixel == '1' ? '#' : ' ');
            }
            putchar('\n');
        }

        free(finalImage);
    }
    return 0;
}

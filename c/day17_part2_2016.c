
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// --- MD5 Implementation (Simplified - common public domain version) ---
// NOTE: In a real scenario, prefer a vetted library like OpenSSL.
// This is a basic implementation for demonstration.

typedef unsigned int MD5_UINT32;

typedef struct {
    MD5_UINT32 state[4];
    MD5_UINT32 count[2];
    unsigned char buffer[64];
} MD5_CTX;

#define S11 7
#define S12 12
#define S13 17
#define S14 22
#define S21 5
#define S22 9
#define S23 14
#define S24 20
#define S31 4
#define S32 11
#define S33 16
#define S34 23
#define S41 6
#define S42 10
#define S43 15
#define S44 21

static unsigned char PADDING[64] = {
  0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

#define F(x, y, z) (((x) & (y)) | ((~x) & (z)))
#define G(x, y, z) (((x) & (z)) | ((y) & (~z)))
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define I(x, y, z) ((y) ^ ((x) | (~z)))

#define ROTATE_LEFT(x, n) (((x) << (n)) | ((x) >> (32-(n))))

#define FF(a, b, c, d, x, s, ac) { \
 (a) += F ((b), (c), (d)) + (x) + (MD5_UINT32)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }
#define GG(a, b, c, d, x, s, ac) { \
 (a) += G ((b), (c), (d)) + (x) + (MD5_UINT32)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }
#define HH(a, b, c, d, x, s, ac) { \
 (a) += H ((b), (c), (d)) + (x) + (MD5_UINT32)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }
#define II(a, b, c, d, x, s, ac) { \
 (a) += I ((b), (c), (d)) + (x) + (MD5_UINT32)(ac); \
 (a) = ROTATE_LEFT ((a), (s)); \
 (a) += (b); \
  }

static void MD5Decode(MD5_UINT32 *output, const unsigned char *input, unsigned int len) {
    unsigned int i, j;
    for (i = 0, j = 0; j < len; i++, j += 4)
        output[i] = ((MD5_UINT32)input[j]) | (((MD5_UINT32)input[j+1]) << 8) |
                   (((MD5_UINT32)input[j+2]) << 16) | (((MD5_UINT32)input[j+3]) << 24);
}

static void MD5Encode(unsigned char *output, const MD5_UINT32 *input, unsigned int len) {
    unsigned int i, j;
    for (i = 0, j = 0; j < len; i++, j += 4) {
        output[j] = (unsigned char)(input[i] & 0xff);
        output[j+1] = (unsigned char)((input[i] >> 8) & 0xff);
        output[j+2] = (unsigned char)((input[i] >> 16) & 0xff);
        output[j+3] = (unsigned char)((input[i] >> 24) & 0xff);
    }
}

static void MD5Transform(MD5_UINT32 state[4], const unsigned char block[64]) {
    MD5_UINT32 a = state[0], b = state[1], c = state[2], d = state[3], x[16];
    MD5Decode (x, block, 64);
    FF (a, b, c, d, x[ 0], S11, 0xd76aa478); FF (d, a, b, c, x[ 1], S12, 0xe8c7b756); FF (c, d, a, b, x[ 2], S13, 0x242070db); FF (b, c, d, a, x[ 3], S14, 0xc1bdceee);
    FF (a, b, c, d, x[ 4], S11, 0xf57c0faf); FF (d, a, b, c, x[ 5], S12, 0x4787c62a); FF (c, d, a, b, x[ 6], S13, 0xa8304613); FF (b, c, d, a, x[ 7], S14, 0xfd469501);
    FF (a, b, c, d, x[ 8], S11, 0x698098d8); FF (d, a, b, c, x[ 9], S12, 0x8b44f7af); FF (c, d, a, b, x[10], S13, 0xffff5bb1); FF (b, c, d, a, x[11], S14, 0x895cd7be);
    FF (a, b, c, d, x[12], S11, 0x6b901122); FF (d, a, b, c, x[13], S12, 0xfd987193); FF (c, d, a, b, x[14], S13, 0xa679438e); FF (b, c, d, a, x[15], S14, 0x49b40821);
    GG (a, b, c, d, x[ 1], S21, 0xf61e2562); GG (d, a, b, c, x[ 6], S22, 0xc040b340); GG (c, d, a, b, x[11], S23, 0x265e5a51); GG (b, c, d, a, x[ 0], S24, 0xe9b6c7aa);
    GG (a, b, c, d, x[ 5], S21, 0xd62f105d); GG (d, a, b, c, x[10], S22,  0x2441453); GG (c, d, a, b, x[15], S23, 0xd8a1e681); GG (b, c, d, a, x[ 4], S24, 0xe7d3fbc8);
    GG (a, b, c, d, x[ 9], S21, 0x21e1cde6); GG (d, a, b, c, x[14], S22, 0xc33707d6); GG (c, d, a, b, x[ 3], S23, 0xf4d50d87); GG (b, c, d, a, x[ 8], S24, 0x455a14ed);
    GG (a, b, c, d, x[13], S21, 0xa9e3e905); GG (d, a, b, c, x[ 2], S22, 0xfcefa3f8); GG (c, d, a, b, x[ 7], S23, 0x676f02d9); GG (b, c, d, a, x[12], S24, 0x8d2a4c8a);
    HH (a, b, c, d, x[ 5], S31, 0xfffa3942); HH (d, a, b, c, x[ 8], S32, 0x8771f681); HH (c, d, a, b, x[11], S33, 0x6d9d6122); HH (b, c, d, a, x[14], S34, 0xfde5380c);
    HH (a, b, c, d, x[ 1], S31, 0xa4beea44); HH (d, a, b, c, x[ 4], S32, 0x4bdecfa9); HH (c, d, a, b, x[ 7], S33, 0xf6bb4b60); HH (b, c, d, a, x[10], S34, 0xbebfbc70);
    HH (a, b, c, d, x[13], S31, 0x289b7ec6); HH (d, a, b, c, x[ 0], S32, 0xeaa127fa); HH (c, d, a, b, x[ 3], S33, 0xd4ef3085); HH (b, c, d, a, x[ 6], S34,  0x4881d05);
    HH (a, b, c, d, x[ 9], S31, 0xd9d4d039); HH (d, a, b, c, x[12], S32, 0xe6db99e5); HH (c, d, a, b, x[15], S33, 0x1fa27cf8); HH (b, c, d, a, x[ 2], S34, 0xc4ac5665);
    II (a, b, c, d, x[ 0], S41, 0xf4292244); II (d, a, b, c, x[ 7], S42, 0x432aff97); II (c, d, a, b, x[14], S43, 0xab9423a7); II (b, c, d, a, x[ 5], S44, 0xfc93a039);
    II (a, b, c, d, x[12], S41, 0x655b59c3); II (d, a, b, c, x[ 3], S42, 0x8f0ccc92); II (c, d, a, b, x[10], S43, 0xffeff47d); II (b, c, d, a, x[ 1], S44, 0x85845dd1);
    II (a, b, c, d, x[ 8], S41, 0x6fa87e4f); II (d, a, b, c, x[15], S42, 0xfe2ce6e0); II (c, d, a, b, x[ 6], S43, 0xa3014314); II (b, c, d, a, x[13], S44, 0x4e0811a1);
    II (a, b, c, d, x[ 4], S41, 0xf7537e82); II (d, a, b, c, x[11], S42, 0xbd3af235); II (c, d, a, b, x[ 2], S43, 0x2ad7d2bb); II (b, c, d, a, x[ 9], S44, 0xeb86d391);
    state[0] += a; state[1] += b; state[2] += c; state[3] += d;
}

void MD5Init(MD5_CTX *context) {
    context->count[0] = context->count[1] = 0;
    context->state[0] = 0x67452301;
    context->state[1] = 0xefcdab89;
    context->state[2] = 0x98badcfe;
    context->state[3] = 0x10325476;
}

void MD5Update(MD5_CTX *context, const unsigned char *input, unsigned int inputLen) {
    unsigned int i, index, partLen;
    index = (unsigned int)((context->count[0] >> 3) & 0x3F);
    if ((context->count[0] += ((MD5_UINT32)inputLen << 3)) < ((MD5_UINT32)inputLen << 3))
        context->count[1]++;
    context->count[1] += ((MD5_UINT32)inputLen >> 29);
    partLen = 64 - index;
    if (inputLen >= partLen) {
        memcpy(&context->buffer[index], input, partLen);
        MD5Transform (context->state, context->buffer);
        for (i = partLen; i + 63 < inputLen; i += 64)
            MD5Transform (context->state, &input[i]);
        index = 0;
    } else
        i = 0;
    memcpy(&context->buffer[index], &input[i], inputLen - i);
}

void MD5Final(unsigned char digest[16], MD5_CTX *context) {
    unsigned char bits[8];
    unsigned int index, padLen;
    MD5Encode (bits, context->count, 8);
    index = (unsigned int)((context->count[0] >> 3) & 0x3f);
    padLen = (index < 56) ? (56 - index) : (120 - index);
    MD5Update (context, PADDING, padLen);
    MD5Update (context, bits, 8);
    MD5Encode (digest, context->state, 16);
}

void bytes_to_hex(const unsigned char *bytes, char *hex_string, size_t bytes_len) {
    for (size_t i = 0; i < bytes_len; i++) {
        sprintf(hex_string + (i * 2), "%02x", bytes[i]);
    }
    hex_string[bytes_len * 2] = '\0';
}

// --- Queue Implementation ---

typedef struct {
    int x;
    int y;
    char *path;
} Point;

typedef struct {
    Point *data;
    int capacity;
    int size;
    int front;
    int rear;
} Queue;

Queue* create_queue(int capacity) {
    Queue *q = (Queue*)malloc(sizeof(Queue));
    if (!q) exit(1);
    q->data = (Point*)malloc(sizeof(Point) * capacity);
    if (!q->data) { free(q); exit(1); }
    q->capacity = capacity;
    q->size = 0;
    q->front = 0;
    q->rear = -1;
    return q;
}

void destroy_queue(Queue *q) {
    if (!q) return;
    // Paths are freed when dequeued or at the end
    free(q->data);
    free(q);
}

bool is_empty(Queue *q) {
    return q->size == 0;
}

void enqueue(Queue *q, Point p) {
    if (q->size == q->capacity) {
        int new_capacity = q->capacity * 2;
        Point *new_data = (Point*)realloc(q->data, sizeof(Point) * new_capacity);
        if (!new_data) exit(1);
        q->data = new_data;
        // Adjust elements if wrap-around occurred
        if (q->rear < q->front) {
            for (int i = 0; i <= q->rear; ++i) {
                 q->data[q->capacity + i] = q->data[i];
            }
            q->rear += q->capacity;
         }
        q->capacity = new_capacity;
    }
    q->rear = (q->rear + 1) % q->capacity;
    q->data[q->rear] = p;
    q->size++;
}

Point dequeue(Queue *q) {
    Point p = q->data[q->front];
    q->front = (q->front + 1) % q->capacity;
    q->size--;
    return p;
}


// --- Main Logic ---

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    char passcode[50]; // Assume passcode won't exceed this length
    if (!fgets(passcode, sizeof(passcode), file)) {
         fprintf(stderr, "Error reading passcode or file empty.\n");
         fclose(file);
         return 1;
    }
    fclose(file);

    // Remove trailing newline if present
    passcode[strcspn(passcode, "\n")] = 0;

    int longest = 0;
    Queue *queue = create_queue(1024); // Initial capacity

    Point start = {0, 0, (char*)malloc(1)};
    if (!start.path) exit(1);
    start.path[0] = '\0';
    enqueue(queue, start);

    int dx[] = {0, 0, -1, 1}; // Corresponds to U, D, L, R
    int dy[] = {-1, 1, 0, 0};
    char dirs[] = "UDLR";

    while (!is_empty(queue)) {
        Point current = dequeue(queue);

        if (current.x == 3 && current.y == 3) {
            int current_len = strlen(current.path);
            if (current_len > longest) {
                longest = current_len;
            }
            free(current.path); // Free path since we reached the target
            continue; // Continue searching for potentially longer paths
        }

        size_t passcode_len = strlen(passcode);
        size_t current_path_len = strlen(current.path);
        size_t input_len = passcode_len + current_path_len;
        char *hash_input = (char*)malloc(input_len + 1);
        if (!hash_input) exit(1);
        sprintf(hash_input, "%s%s", passcode, current.path);

        unsigned char digest[16];
        MD5_CTX md5_context;
        MD5Init(&md5_context);
        MD5Update(&md5_context, (unsigned char*)hash_input, input_len);
        MD5Final(digest, &md5_context);
        free(hash_input);

        char hex_digest[33];
        bytes_to_hex(digest, hex_digest, 16);

        for (int i = 0; i < 4; ++i) { // Check U, D, L, R doors
            char door_check = hex_digest[i];
            if (door_check >= 'b' && door_check <= 'f') { // Door is open
                int next_x = current.x + dx[i];
                int next_y = current.y + dy[i];

                if (next_x >= 0 && next_x < 4 && next_y >= 0 && next_y < 4) {
                    size_t next_path_len = current_path_len + 1;
                    char *next_path = (char*)malloc(next_path_len + 1);
                    if (!next_path) exit(1);
                    sprintf(next_path, "%s%c", current.path, dirs[i]);

                    Point next = {next_x, next_y, next_path};
                    enqueue(queue, next);
                }
            }
        }
        free(current.path); // Free the current path after exploring neighbors
    }

    printf("%d\n", longest);
    destroy_queue(queue);

    return 0;
}

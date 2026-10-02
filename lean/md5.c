#include <lean/lean.h>
#include <openssl/evp.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

static int has_zero_prefix(const unsigned char *digest, unsigned int zeros) {
    unsigned int full_bytes = zeros / 2;
    unsigned int half_byte = zeros % 2;

    for (unsigned int i = 0; i < full_bytes; i++) {
        if (digest[i] != 0)
            return 0;
    }

    if (half_byte) {
        if ((digest[full_bytes] & 0xF0) != 0)
            return 0;
    }

    return 1;
}

LEAN_EXPORT lean_obj_res lean_find_coin(
    b_lean_obj_arg secret_obj,
    uint32_t zeros
) {
    const char *secret = lean_string_cstr(secret_obj);
    size_t secret_len = strlen(secret);

    EVP_MD_CTX *ctx = EVP_MD_CTX_new();

    unsigned char digest[EVP_MAX_MD_SIZE];
    unsigned int digest_len;

    char input[256];

    for (uint64_t i = 1; ; i++) {
        int len = snprintf(
            input,
            sizeof(input),
            "%s%llu",
            secret,
            (unsigned long long)i
        );

        EVP_DigestInit_ex(ctx, EVP_md5(), NULL);
        EVP_DigestUpdate(ctx, input, (size_t)len);
        EVP_DigestFinal_ex(ctx, digest, &digest_len);

        if (has_zero_prefix(digest, zeros)) {
            EVP_MD_CTX_free(ctx);
            return lean_box((size_t)i);
        }
    }
}

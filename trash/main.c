#include <gpgme/gpgme.h>
#include <gpg-error.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <locale.h>
#include <unistd.h>

// #define INSERT_FAILURE

int bang(const gpgme_error_t err) {
    fprintf(stderr, "%s: %s\n", gpgme_strerror(err), gpgme_strsource(err));
    return err;
}
int bang_(const char *e) {
    fprintf(stderr, "%s\n", e);
    return 1;
}

int main(void) {
    gpgme_check_version (NULL);

    gpgme_error_t err;
    gpgme_data_t plain, cipher;
    gpgme_ctx_t ctx;
    gpgme_key_t recp[2] = { NULL, NULL };
    gpgme_encrypt_flags_t flags = GPGME_ENCRYPT_ALWAYS_TRUST;

    char *plaintext = "foo bar\0";
    char *fp = "845B80B9AD12DB400CE534F6837EED10F97A36A1";
    char *result_file = "./result.gpg";
    char *verify_file = "./result";
    size_t max_buflen = 2048, buflen;
    char *buf = malloc(max_buflen * sizeof(char));
    FILE *fh = NULL;

    err = gpgme_new (&ctx);
    if(err) return bang (err);

    gpgme_set_armor (ctx, 1);

    err = gpgme_get_key (ctx, fp, &recp[0], 0);
    if(err) return bang (err);

    err = gpgme_data_new_from_mem (&plain, plaintext, strlen(plaintext), 0);
    if(err) return bang (err);

    err = gpgme_data_new (&cipher);
    if(err) return bang (err);

    err = gpgme_op_encrypt (ctx, recp, flags, plain, cipher);
    if(err) return bang (err);

    gpgme_data_seek(cipher, 0, SEEK_SET);
    buflen = gpgme_data_read(cipher, buf, max_buflen);
    if (1 > buflen || buflen == max_buflen)
        return bang_ ("Failed to read ciphertext");

    fh = fopen(result_file, "w");
    if(!fh) bang_ ("failed to open result_file");

    fwrite(buf, sizeof(char), buflen, fh);
    fclose(fh);
    fh = NULL;

    memset(buf, 0, max_buflen);
    snprintf(buf, max_buflen-1, "gpg --output %s -d %s", verify_file, result_file);
    system(buf);

    memset(buf, 0, max_buflen);
    fh = fopen(verify_file, "rb");
    if(!fh) return bang_ ("failed to open verify_file");

    buflen = fread(buf, sizeof(char), max_buflen, fh);
    fclose(fh);

    if(buflen < 1 || buflen == max_buflen)
        return bang_ ("Failed to read result file");

    #ifdef INSERT_FAILURE
    buf[buflen-1] = '\0';
    #endif

    if (strncmp(buf, plaintext, strlen(plaintext)) != 0)
        return bang_ ("Decrypted text is different from original plaintext");

    return 0;
}

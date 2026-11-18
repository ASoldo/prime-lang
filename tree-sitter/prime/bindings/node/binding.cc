#include <napi.h>

typedef struct TSLanguage TSLanguage;

extern "C" const TSLanguage *tree_sitter_prime(void);

// "tree-sitter", "language" hashed with BLAKE2
const napi_type_tag LANGUAGE_TYPE_TAG = {
    0x8AF2E5212AD58ABF, 0xD5006CAD83ABBA16
};

Napi::Object Init(Napi::Env env, Napi::Object exports) {
    auto language = Napi::External<TSLanguage>::New(env, const_cast<TSLanguage *>(tree_sitter_prime()));
    language.TypeTag(&LANGUAGE_TYPE_TAG);
    exports["language"] = language;
    return exports;
}

NODE_API_MODULE(tree_sitter_prime_lang_binding, Init)

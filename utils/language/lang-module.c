#include <string.h>
#include <stdbool.h>
#include <tree_sitter/runtime.h>
#include <emacs-module.h>

// TODO: If your plugin is GPL compatible, uncomment the below
// int plugin_is_GPL_compatible;

typedef struct TSElLanguage {
  // Tag to check type, must be "TSLanguage" followed by null char.
  char tag[11];
  TSLanguage *ptr;
} TSElLanguage;

TSElLanguage lang;

static bool pending_nonlocal_exit(emacs_env *env) {
  return env->non_local_exit_check(env) != emacs_funcall_exit_return;
}

char *docstring = "Return the tree-sitter language object for LANG_NAME.";
static emacs_value tree_sitter_get_lang(emacs_env *env,
                                        __attribute__((unused)) ptrdiff_t nargs,
                                        __attribute__((unused)) emacs_value *args,
                                        __attribute__((unused)) void *data) {
  emacs_value Qts_language_create = env->intern(env, "tree-sitter-language--create");
  emacs_value user_ptr = env->make_user_ptr(env, NULL, &lang);
  emacs_value func_args[1] = { user_ptr };
  return env->funcall(env, Qts_language_create, 1, func_args);
}

TSLanguage *tree_sitter_LANG_NAME();

int emacs_module_init(struct emacs_runtime *ert) {
  // Fill in the language struct
  strncpy(lang.tag, "TSLanguage", 11);
  lang.ptr = tree_sitter_LANG_NAME();
  emacs_env *env = ert->get_environment(ert);
  // Require the definitions
  emacs_value require_feature = env->intern(env, "tree-sitter-defs");
  emacs_value Qrequire = env->intern(env, "require");
  emacs_value args[2] = { require_feature, NULL};
  env->funcall(env, Qrequire, 1, args);
  if(pending_nonlocal_exit(env)) {
    return 1;
  }
  // Provide the function
  emacs_value lang_func = env->make_function(env, 0, 0, &tree_sitter_get_lang, docstring, NULL);
  emacs_value Qdefalias = env->intern(env, "defalias");
  emacs_value Qlang_func = env->intern(env, "tree-sitter-lang-LANG_NAME");
  args[0] = Qlang_func;
  args[1] = lang_func;
  env->funcall(env, Qdefalias, 2, args);
  if(pending_nonlocal_exit(env)) {
    return 2;
  }
  // Provide the module
  emacs_value feature_name = env->intern(env, "tree-sitter-lang-LANG_NAME-module");
  emacs_value provide_symbol = env->intern(env, "provide");
  args[0] = feature_name;
  env->funcall(env, provide_symbol, 1, args);
  if(pending_nonlocal_exit(env)) {
    return 3;
  }
  return 0;
}

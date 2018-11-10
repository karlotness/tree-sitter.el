/*
 * Copyright (C) 2018 Karl Otness
 *
 * This file is part of tree-sitter.el.
 *
 * tree-sitter.el is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * tree-sitter.el is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with tree-sitter.el. If not, see
 * <https://www.gnu.org/licenses/>.
 */
#include <string.h>
#include <stdlib.h>
#include "language.h"
#include "symbol.h"
#include "common.h"

static const char *tsel_language_symbol_count_doc = "Count the number of symbols in LANG.\n"
  "LANG is a `tree-sitter-language-p' object.\n"
  "\n"
  "(fn LANG)";
static emacs_value tsel_language_symbol_count(emacs_env *env,
                                              __attribute__((unused)) ptrdiff_t nargs,
                                              emacs_value *args,
                                              __attribute__((unused)) void *data) {
  if(!tsel_language_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-language-p", args[0]);
    return tsel_Qnil;
  }
  TSElLanguage *ptr = tsel_language_get_ptr(env, args[0]);
  if(tsel_pending_nonlocal_exit(env)) {
    return tsel_Qnil;
  }
  uint32_t s_count = ts_language_symbol_count(ptr->ptr);
  return env->make_integer(env, s_count);
}

static const char *tsel_language_symbol_name_doc = "Retrieve the name of SYMBOL under LANG.\n"
  "LANG is a tree-sitter-language and SYMBOL is a tree-sitter-symbol.\n"
  "Returns the symbol name as a string or nil, if it is not defined.\n"
  "\n"
  "(fn LANG SYMBOL)";
static emacs_value tsel_language_symbol_name(emacs_env *env,
                                             __attribute__((unused)) ptrdiff_t nargs,
                                             emacs_value *args,
                                             __attribute__((unused)) void *data) {
  if(!tsel_language_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-language-p", args[0]);
    return tsel_Qnil;
  }
  else if(!tsel_symbol_p(env, args[1])) {
    tsel_signal_wrong_type(env, "tree-sitter-symbol-p", args[1]);
    return tsel_Qnil;
  }
  TSLanguage *lang = tsel_language_get_ptr(env, args[0])->ptr;
  TSSymbol code = 0;
  if(!tsel_symbol_get_code(env, args[1], &code) || tsel_pending_nonlocal_exit(env)) {
    return tsel_Qnil;
  }
  else if(code < ts_language_symbol_count(lang)) {
    const char *name = ts_language_symbol_name(lang, code);
    return env->make_string(env, name, strlen(name));
  }
  return tsel_Qnil;
}

static const char *tsel_language_symbol_for_name_doc = "Retrieve a symbol from LANG by its NAME.\n"
  "LANG is a tree-sitter-language and NAME is a string naming a symbol.\n"
  "Returns the symbol or nil if it was not found.\n"
  "\n"
  "(fn LANG NAME)";
static emacs_value tsel_language_symbol_for_name(emacs_env *env,
                                                 __attribute__((unused)) ptrdiff_t nargs,
                                                 emacs_value *args,
                                                 __attribute__((unused)) void *data) {
  if(!tsel_language_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-language-p", args[0]);
    return tsel_Qnil;
  }
  else if(!tsel_string_p(env, args[1])) {
    tsel_signal_wrong_type(env, "stringp", args[1]);
    return tsel_Qnil;
  }
  TSLanguage *lang = tsel_language_get_ptr(env, args[0])->ptr;
  if(tsel_pending_nonlocal_exit(env)) {
    return tsel_Qnil;
  }
  char *str;
  TSEL_SUBR_EXTRACT(string, env, args[1], &str);
  TSSymbol symbol = ts_language_symbol_for_name(lang, str);
  emacs_value res;
  free(str);
  str = NULL;
  if(symbol == 0 || !tsel_symbol_create(env, symbol, &res)) {
    return tsel_Qnil;
  }
  return res;
}

static const char *tsel_language_symbol_type_doc = "Return the type of SYMBOL.\n"
  "Type type of SYMBOL is determined under the tree-sitter-language LANG.\n"
  "This will be one of 'regular, 'anonymous, or 'auxiliary. The value nil may\n"
  "be returned for unknown types, but this should occur only under a version mismatch\n"
  "with the underlying tree-sitter library.\n"
  "\n"
  "(fn LANG SYMBOL)";
static emacs_value tsel_language_symbol_type(emacs_env *env,
                                             __attribute__((unused)) ptrdiff_t nargs,
                                             emacs_value *args,
                                             __attribute__((unused)) void *data) {
  if(!tsel_language_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-language-p", args[0]);
    return tsel_Qnil;
  }
  else if(!tsel_symbol_p(env, args[1])) {
    tsel_signal_wrong_type(env, "tree-sitter-symbol-p", args[1]);
    return tsel_Qnil;
  }
  TSLanguage *lang = tsel_language_get_ptr(env, args[0])->ptr;
  TSSymbol symbol;
  if(!lang || !tsel_symbol_get_code(env, args[1], &symbol) ||
     tsel_pending_nonlocal_exit(env)) {
    return tsel_Qnil;
  }
  TSSymbolType type = ts_language_symbol_type(lang, symbol);
  if(type == TSSymbolTypeRegular) {
    return env->intern(env, "regular");
  }
  else if(type == TSSymbolTypeAnonymous) {
    return env->intern(env, "anonymous");
  }
  else if(type == TSSymbolTypeAuxiliary) {
    return env->intern(env, "auxiliary");
  }
  return tsel_Qnil;
}

static const char *tsel_language_version_doc = "Return the version of language LANG.\n"
  "\n"
  "(fn LANG)";
static emacs_value tsel_language_version(emacs_env *env,
                                             __attribute__((unused)) ptrdiff_t nargs,
                                             emacs_value *args,
                                             __attribute__((unused)) void *data) {
  if(!tsel_language_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-language-p", args[0]);
    return tsel_Qnil;
  }
  TSLanguage *lang = tsel_language_get_ptr(env, args[0])->ptr;
  if(!lang) {
    return tsel_Qnil;
  }
  return env->make_integer(env, ts_language_version(lang));
}

static const char *tsel_language_p_wrapped_doc = "Return t if OBJECT is a tree-sitter-language.\n"
  "\n"
  "(fn OBJECT)";
static emacs_value tsel_language_p_wrapped(emacs_env *env,
                                           __attribute__((unused)) ptrdiff_t nargs,
                                           emacs_value *args,
                                           __attribute__((unused)) void *data) {
  if(tsel_language_p(env, args[0])) {
    return tsel_Qt;
  }
  return tsel_Qnil;
}

bool tsel_language_init(emacs_env *env) {
  bool function_result = tsel_define_function(env, "tree-sitter-language-symbol-count",
                                              &tsel_language_symbol_count, 1, 1,
                                              tsel_language_symbol_count_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-language-symbol-name",
                                          &tsel_language_symbol_name, 2, 2,
                                          tsel_language_symbol_name_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-language-symbol-for-name",
                                          &tsel_language_symbol_for_name, 2, 2,
                                          tsel_language_symbol_for_name_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-language-symbol-type",
                                          &tsel_language_symbol_type, 2, 2,
                                          tsel_language_symbol_type_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-language-version",
                                          &tsel_language_version, 1, 1,
                                          tsel_language_version_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-language-p",
                                          &tsel_language_p_wrapped, 1, 1,
                                          tsel_language_p_wrapped_doc, NULL);
  return function_result;
}

bool tsel_language_p(emacs_env *env, emacs_value obj) {
  if(!tsel_check_record_type(env, "tree-sitter-language", obj)) {
    return false;
  }
  // Get the ptr field
  emacs_value user_ptr;
  if(!tsel_record_get_field(env, obj, 1, &user_ptr)) {
    return false;
  }
  // Make sure it's a user pointer
  emacs_value Quser_ptrp = env->intern(env, "user-ptrp");
  emacs_value args[1] = { user_ptr };
  if(!env->eq(env, env->funcall(env, Quser_ptrp, 1, args), tsel_Qt) ||
     tsel_pending_nonlocal_exit(env)) {
    return false;
  }
  // Get the raw pointer
  TSElLanguage *ptr = env->get_user_ptr(env, user_ptr);
  if(ptr == NULL || tsel_pending_nonlocal_exit(env)) {
    return false;
  }
  // Check the type tag in the struct
  return strncmp(ptr->tag, "TSLanguage", 11) == 0;
}

TSElLanguage *tsel_language_get_ptr(emacs_env *env, emacs_value obj) {
  if(!tsel_language_p(env, obj)) {
    return NULL;
  }
    // Get the ptr field
  emacs_value user_ptr;
  if(!tsel_record_get_field(env, obj, 1, &user_ptr)) {
    return NULL;
  }
  // Get the raw pointer
  TSElLanguage *ptr = env->get_user_ptr(env, user_ptr);
  if(tsel_pending_nonlocal_exit(env)) {
    return NULL;
  }
  return ptr;
}

emacs_value tsel_language_wrap(emacs_env *env, TSElLanguage *lang) {
  emacs_value Qts_language_create = env->intern(env, "tree-sitter-language--create");
  emacs_value user_ptr = env->make_user_ptr(env, NULL, lang);
  emacs_value func_args[1] = { user_ptr };
  return env->funcall(env, Qts_language_create, 1, func_args);
}

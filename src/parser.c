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
#include <stdlib.h>
#include "parser.h"
#include "common.h"

static void tsel_parser_fin(void *ptr) {
  TSElParser *parser = ptr;
  ts_parser_delete(parser->parser);
  free(parser);
}

char *tsel_parser_new_doc = "Create a new tree-sitter parser.\n";
static emacs_value tsel_parser_new(emacs_env *env,
                                   __attribute__((unused)) ptrdiff_t nargs,
                                   __attribute__((unused)) emacs_value *args,
                                   __attribute__((unused)) void *data) {
  TSElParser *wrapper = malloc(sizeof(TSElParser));
  TSParser *parser = ts_parser_new();
  if(!wrapper || !parser) {
    if(wrapper) {
      free(wrapper);
    }
    if(parser) {
      ts_parser_delete(parser);
    }
    tsel_signal_error(env, "Initialization failed");
    return tsel_Qnil;
  }
  wrapper->parser = parser;
  wrapper->lang = NULL;
  emacs_value new_parser = env->make_user_ptr(env, &tsel_parser_fin, wrapper);
  emacs_value Qts_parser_create = env->intern(env, "tree-sitter-parser--create");
  emacs_value funargs[1] = { new_parser };
  emacs_value res = env->funcall(env, Qts_parser_create, 1, funargs);
  if(tsel_pending_nonlocal_exit(env)) {
    tsel_parser_fin(wrapper);
    tsel_signal_error(env, "Initialization failed");
    return tsel_Qnil;
  }
  return res;
}

char *tsel_parser_p_wrapped_doc = "Return t if OBJECT is a tree-sitter-parser.\n"
  "\n"
  "(fn OBJECT)";
static emacs_value tsel_parser_p_wrapped(emacs_env *env,
                                         __attribute__((unused)) ptrdiff_t nargs,
                                         emacs_value *args,
                                         __attribute__((unused)) void *data) {
  if(tsel_parser_p(env, args[0])) {
    return tsel_Qt;
  }
  return tsel_Qnil;
}

bool tsel_parser_p(emacs_env *env, emacs_value obj) {
  if(!tsel_check_record_type(env, "tree-sitter-parser", obj)) {
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
  // Check the finalizer
  emacs_finalizer *fin = env->get_user_finalizer(env, user_ptr);
  return !tsel_pending_nonlocal_exit(env) && fin == &tsel_parser_fin;
}

bool tsel_parser_init(emacs_env *env) {
  bool function_result = tsel_define_function(env, "tree-sitter-parser-new",
                                              &tsel_parser_new, 0, 0,
                                              tsel_parser_new_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-parser-p",
                                          &tsel_parser_p_wrapped, 1, 1,
                                          tsel_parser_p_wrapped_doc, NULL);
  return function_result;
}

TSElParser *tsel_parser_get_ptr(emacs_env *env, emacs_value obj) {
  if(!tsel_parser_p(env, obj)) {
    return NULL;
  }
  // Get the ptr field
  emacs_value user_ptr;
  if(!tsel_record_get_field(env, obj, 1, &user_ptr)) {
    return NULL;
  }
  // Get the raw pointer
  TSElParser *ptr = env->get_user_ptr(env, user_ptr);
  if(tsel_pending_nonlocal_exit(env)) {
    return NULL;
  }
  return ptr;
}

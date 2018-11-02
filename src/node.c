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
#include <string.h>
#include "node.h"
#include "common.h"
#include "symbol.h"
#include "point.h"

static void tsel_node_fin(void *ptr) {
  TSElNode *node = ptr;
  tsel_node_free(node);
}

static char *tsel_node_p_wrapped_doc = "Return t if OBJECT is a tree-sitter-node.\n"
  "\n"
  "(fn OBJECT)";
static emacs_value tsel_node_p_wrapped(emacs_env *env,
                                       __attribute__((unused)) ptrdiff_t nargs,
                                       emacs_value *args,
                                       __attribute__((unused)) void *data) {
  if(tsel_node_p(env, args[0])) {
    return tsel_Qt;
  }
  return tsel_Qnil;
}

static char *tsel_node_symbol_doc = "Return the symbol of node NODE\n"
  "\n"
  "(fn NODE)";
static emacs_value tsel_node_symbol(emacs_env *env,
                                    __attribute__((unused)) ptrdiff_t nargs,
                                    emacs_value *args,
                                    __attribute__((unused)) void *data) {
  if(!tsel_node_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-node-p", args[0]);
    return tsel_Qnil;
  }
  TSElNode *node = tsel_node_get_ptr(env, args[0]);
  if(!node || tsel_pending_nonlocal_exit(env)) {
    tsel_signal_error(env, "Failed to retrieve node.");
    return tsel_Qnil;
  }
  TSSymbol symb = ts_node_symbol(node->node);
  emacs_value obj;
  if(!tsel_symbol_create(env, symb, &obj)) {
    tsel_signal_error(env, "Allocation failed.");
    return tsel_Qnil;
  }
  return obj;
}

static char *tsel_node_type_doc = "Return the type of node NODE.\n"
  "The node's type is a string, the same as the name of its symbol\n"
  "under the tree's language. See `tree-sitter-language-symbol-name'.\n"
  "\n"
  "(fn NODE)";
static emacs_value tsel_node_type(emacs_env *env,
                                  __attribute__((unused)) ptrdiff_t nargs,
                                    emacs_value *args,
                                    __attribute__((unused)) void *data) {
  if(!tsel_node_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-node-p", args[0]);
    return tsel_Qnil;
  }
  TSElNode *node = tsel_node_get_ptr(env, args[0]);
  if(!node || tsel_pending_nonlocal_exit(env)) {
    tsel_signal_error(env, "Failed to retrieve node.");
    return tsel_Qnil;
  }
  const char *name = ts_node_type(node->node);
  emacs_value str = env->make_string(env, name, strlen(name));
  return str;
}

static char *tsel_node_start_byte_doc = "Return the starting byte of a tree-sitter node.\n"
  "\n"
  "(fn NODE)";
static emacs_value tsel_node_start_byte(emacs_env *env,
                                  __attribute__((unused)) ptrdiff_t nargs,
                                    emacs_value *args,
                                    __attribute__((unused)) void *data) {
  if(!tsel_node_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-node-p", args[0]);
    return tsel_Qnil;
  }
  TSElNode *node = tsel_node_get_ptr(env, args[0]);
  if(!node || tsel_pending_nonlocal_exit(env)) {
    tsel_signal_error(env, "Failed to retrieve node.");
    return tsel_Qnil;
  }
  uint32_t byte = ts_node_start_byte(node->node);
  return env->make_integer(env, byte + 1);
}

static char *tsel_node_end_byte_doc = "Return the ending byte of a tree-sitter node.\n"
  "\n"
  "(fn NODE)";
static emacs_value tsel_node_end_byte(emacs_env *env,
                                  __attribute__((unused)) ptrdiff_t nargs,
                                    emacs_value *args,
                                    __attribute__((unused)) void *data) {
  if(!tsel_node_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-node-p", args[0]);
    return tsel_Qnil;
  }
  TSElNode *node = tsel_node_get_ptr(env, args[0]);
  if(!node || tsel_pending_nonlocal_exit(env)) {
    tsel_signal_error(env, "Failed to retrieve node.");
    return tsel_Qnil;
  }
  uint32_t byte = ts_node_end_byte(node->node);
  return env->make_integer(env, byte + 1);
}

static char *tsel_node_start_point_doc = "Return the starting point of NODE.\n"
  "The point is a pair of row and column collected into a\n"
  "tree-sitter-point record.\n"
  "\n"
  "(fn NODE)";
static emacs_value tsel_node_start_point(emacs_env *env,
                                  __attribute__((unused)) ptrdiff_t nargs,
                                    emacs_value *args,
                                    __attribute__((unused)) void *data) {
  if(!tsel_node_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-node-p", args[0]);
    return tsel_Qnil;
  }
  TSElNode *node = tsel_node_get_ptr(env, args[0]);
  if(!node || tsel_pending_nonlocal_exit(env)) {
    tsel_signal_error(env, "Failed to retrieve node.");
    return tsel_Qnil;
  }
  TSPoint point = ts_node_start_point(node->node);
  return tsel_point_emacs_move(env, &point);
}

static char *tsel_node_end_point_doc = "Return the ending point of NODE.\n"
  "The point is a pair of row and column collected into a\n"
  "tree-sitter-point record.\n"
  "\n"
  "(fn NODE)";
static emacs_value tsel_node_end_point(emacs_env *env,
                                  __attribute__((unused)) ptrdiff_t nargs,
                                    emacs_value *args,
                                    __attribute__((unused)) void *data) {
  if(!tsel_node_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-node-p", args[0]);
    return tsel_Qnil;
  }
  TSElNode *node = tsel_node_get_ptr(env, args[0]);
  if(!node || tsel_pending_nonlocal_exit(env)) {
    tsel_signal_error(env, "Failed to retrieve node.");
    return tsel_Qnil;
  }
  TSPoint point = ts_node_end_point(node->node);
  return tsel_point_emacs_move(env, &point);
}

bool tsel_node_init(emacs_env *env) {
  bool function_result = tsel_define_function(env, "tree-sitter-node-p",
                                              &tsel_node_p_wrapped, 1, 1,
                                              tsel_node_p_wrapped_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-symbol",
                                          &tsel_node_symbol, 1, 1,
                                          tsel_node_symbol_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-type",
                                          &tsel_node_type, 1, 1,
                                          tsel_node_type_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-start-byte",
                                          &tsel_node_start_byte, 1, 1,
                                          tsel_node_start_byte_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-end-byte",
                                          &tsel_node_end_byte, 1, 1,
                                          tsel_node_end_byte_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-start-point",
                                          &tsel_node_start_point, 1, 1,
                                          tsel_node_start_point_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-end-point",
                                          &tsel_node_end_point, 1, 1,
                                          tsel_node_end_point_doc, NULL);
  return function_result;
}

TSElNode *tsel_node_wrap(TSNode node, TSElTree *tree) {
  TSElNode *new = malloc(sizeof(TSElNode));
  if(!new) {
    return NULL;
  }
  tsel_tree_retain(tree);
  new->tree = tree;
  new->node = node;
  return new;
}

void tsel_node_free(TSElNode *node) {
  if(!node) {
    return;
  }
  tsel_tree_release(node->tree);
  free(node);
}

emacs_value tsel_node_emacs_move(emacs_env *env, TSElNode *tree) {
  emacs_value Qts_language_create = env->intern(env, "tree-sitter-node--create");
  emacs_value user_ptr = env->make_user_ptr(env, &tsel_node_fin, tree);
  emacs_value func_args[1] = { user_ptr };
  return env->funcall(env, Qts_language_create, 1, func_args);
}

bool tsel_node_p(emacs_env *env, emacs_value obj) {
  if(!tsel_check_record_type(env, "tree-sitter-node", obj)) {
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
  return !tsel_pending_nonlocal_exit(env) && fin == &tsel_node_fin;
}

TSElNode *tsel_node_get_ptr(emacs_env *env, emacs_value obj) {
  if(!tsel_node_p(env, obj)) {
    return NULL;
  }
  // Get the ptr field
  emacs_value user_ptr;
  if(!tsel_record_get_field(env, obj, 1, &user_ptr)) {
    return NULL;
  }
  // Get the raw pointer
  TSElNode *ptr = env->get_user_ptr(env, user_ptr);
  if(tsel_pending_nonlocal_exit(env)) {
    return NULL;
  }
  return ptr;
}

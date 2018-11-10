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

static bool tsel_named_nodes(emacs_env *env, emacs_value arg) {
  return env->eq(env, arg, env->intern(env, "named"));
}

static const char *tsel_node_p_wrapped_doc = "Return t if OBJECT is a tree-sitter-node.\n"
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

static const char *tsel_node_symbol_doc = "Return the symbol of node NODE\n"
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

static const char *tsel_node_type_doc = "Return the type of node NODE.\n"
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

static const char *tsel_node_start_byte_doc = "Return the starting byte of a tree-sitter node.\n"
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

static const char *tsel_node_end_byte_doc = "Return the ending byte of a tree-sitter node.\n"
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

static const char *tsel_node_start_point_doc = "Return the starting point of NODE.\n"
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

static const char *tsel_node_end_point_doc = "Return the ending point of NODE.\n"
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

static const char *tsel_node_eq_doc = "Return non-nil if tree-sitter-node A is equal to B.\n"
  "Both arguments must be tree-sitter-node records.\n"
  "\n"
  "(fn A B)";
static emacs_value tsel_node_eq(emacs_env *env,
                                __attribute__((unused)) ptrdiff_t nargs,
                                emacs_value *args,
                                __attribute__((unused)) void *data) {
  TSElNode *nodes[2];
  for(int i = 0; i < 2; i++) {
    if(!tsel_node_p(env, args[i])) {
      tsel_signal_wrong_type(env, "tree-sitter-node-p", args[i]);
      return tsel_Qnil;
    }
    nodes[i] = tsel_node_get_ptr(env, args[i]);
    if(!nodes[i] || tsel_pending_nonlocal_exit(env)) {
      tsel_signal_error(env, "Failed to retrieve node.");
      return tsel_Qnil;
    }
  }
  if(ts_node_eq(nodes[0]->node, nodes[0]->node)) {
    return tsel_Qt;
  }
  return tsel_Qnil;
}

static bool (*tsel_node_predicates[4]) (TSNode) = {&ts_node_is_named, &ts_node_is_missing, &ts_node_has_changes, &ts_node_has_error};

static emacs_value tsel_node_predicate(emacs_env *env,
                                       __attribute__((unused)) ptrdiff_t nargs,
                                       emacs_value *args,
                                       void *data) {
  bool (**tsel_node_function) (TSNode) = data;
  if(!tsel_node_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-node-p", args[0]);
    return tsel_Qnil;
  }
  TSElNode *node = tsel_node_get_ptr(env, args[0]);
  if(!node || tsel_pending_nonlocal_exit(env)) {
    tsel_signal_error(env, "Failed to retrieve node.");
    return tsel_Qnil;
  }
  if((*tsel_node_function)(node->node)) {
    return tsel_Qt;
  }
  return tsel_Qnil;
}

static const char *tsel_node_is_named_doc = "Return non-nil if NODE is named.\n"
  "\n"
  "(fn NODE)";
static const char *tsel_node_is_missing_doc = "Return non-nil if NODE is missing.\n"
  "\n"
  "(fn NODE)";
static const char *tsel_node_has_changes_doc = "Return non-nil if NODE has changes.\n"
  "\n"
  "(fn NODE)";
static const char *tsel_node_has_error_doc = "Return non-nil if NODE has an error.\n"
  "\n"
  "(fn NODE)";

static const char *tsel_node_parent_doc = "Return the parent of NODE.\n"
  "\n"
  "(fn NODE)";
static emacs_value tsel_node_parent(emacs_env *env,
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
  TSNode parent = ts_node_parent(node->node);
  TSElNode *wrapped = tsel_node_wrap(parent, node->tree);
  if(!wrapped) {
    tsel_signal_error(env, "Allocation failed.");
    return tsel_Qnil;
  }
  return tsel_node_emacs_move(env, wrapped);
}

static const char *tsel_node_child_count_doc = "Return the number of children of NODE.\n"
  "If TYPE is nil, t, or unspecified count all children. Otherwise if\n"
  "TYPE is the symbol 'named count only named children.\n"
  "The behavior of other values for TYPE is unspecified and may change.\n"
  "\n"
  "(fn NODE &optional TYPE)";
static emacs_value tsel_node_child_count(emacs_env *env,
                                         ptrdiff_t nargs,
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
  bool count_named = nargs > 1 && tsel_named_nodes(env, args[1]);
  if(count_named) {
    return env->make_integer(env, ts_node_named_child_count(node->node));
  }
  return env->make_integer(env, ts_node_child_count(node->node));
}

static const char *tsel_node_child_doc = "Return child of NODE with index IDX.\n"
  "If TYPE is nil, t, or unspecified include all children. Otherwise, if\n"
  "TYPE is the symbol 'named include only named children.\n"
  "The behavior of other values for TYPE is unspecified and may change.\n"
  "\n"
  "(fn NODE IDX &optional TYPE)";
static emacs_value tsel_node_child(emacs_env *env,
                                   ptrdiff_t nargs,
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
  if(!tsel_integer_p(env, args[1])) {
    tsel_signal_wrong_type(env, "integerp", args[1]);
    return tsel_Qnil;
  }
  intmax_t num;
  TSEL_SUBR_EXTRACT(integer, env, args[1], &num);
  if(tsel_pending_nonlocal_exit(env)) {
    return tsel_Qnil;
  }
  bool count_named = nargs > 2 && tsel_named_nodes(env, args[2]);
  TSNode child;
  if(count_named) {
    child = ts_node_named_child(node->node, num);
  }
  else {
    child = ts_node_child(node->node, num);
  }
  TSElNode *wrapped = tsel_node_wrap(child, node->tree);
  if(!wrapped) {
    tsel_signal_error(env, "Allocation failed.");
    return tsel_Qnil;
  }
  return tsel_node_emacs_move(env, wrapped);
}

static const char *tsel_node_next_sibling_doc = "Return next sibling of NODE.\n"
  "If TYPE is nil, t, or unspecified include all siblings. Otherwise, if\n"
  "TYPE is the symbol 'named include only named siblings.\n"
  "The behavior of other values for TYPE is unspecified and may change.\n"
  "\n"
  "(fn NODE &optional TYPE)";
static emacs_value tsel_node_next_sibling(emacs_env *env,
                                          ptrdiff_t nargs,
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
  bool count_named = nargs > 1 && tsel_named_nodes(env, args[1]);
  TSNode sibling;
  if(count_named) {
    sibling = ts_node_next_named_sibling(node->node);
  }
  else {
    sibling = ts_node_next_sibling(node->node);
  }
  TSElNode *wrapped = tsel_node_wrap(sibling, node->tree);
  if(!wrapped) {
    tsel_signal_error(env, "Allocation failed.");
    return tsel_Qnil;
  }
  return tsel_node_emacs_move(env, wrapped);
}

static const char *tsel_node_prev_sibling_doc = "Return previous sibling of NODE.\n"
  "If TYPE is nil, t, or unspecified include all siblings. Otherwise, if\n"
  "TYPE is the symbol 'named include only named siblings.\n"
  "The behavior of other values for TYPE is unspecified and may change.\n"
  "\n"
  "(fn NODE &optional TYPE)";
static emacs_value tsel_node_prev_sibling(emacs_env *env,
                                          ptrdiff_t nargs,
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
  bool count_named = nargs > 1 && tsel_named_nodes(env, args[1]);
  TSNode sibling;
  if(count_named) {
    sibling = ts_node_prev_named_sibling(node->node);
  }
  else {
    sibling = ts_node_prev_sibling(node->node);
  }
  TSElNode *wrapped = tsel_node_wrap(sibling, node->tree);
  if(!wrapped) {
    tsel_signal_error(env, "Allocation failed.");
    return tsel_Qnil;
  }
  return tsel_node_emacs_move(env, wrapped);
}

static const char *tsel_node_first_child_for_byte_doc = "Return first child of NODE for BYTE.\n"
  "If TYPE is nil, t, or unspecified include all siblings. Otherwise, if\n"
  "TYPE is the symbol 'named include only named siblings.\n"
  "The behavior of other values for TYPE is unspecified and may change.\n"
  "\n"
  "(fn NODE BYTE &optional TYPE)";
static emacs_value tsel_node_first_child_for_byte(emacs_env *env,
                                                  ptrdiff_t nargs,
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
  if(!tsel_integer_p(env, args[1])) {
    tsel_signal_wrong_type(env, "integerp", args[1]);
    return tsel_Qnil;
  }
  intmax_t byte;
  TSEL_SUBR_EXTRACT(integer, env, args[1], &byte);
  byte--;
  if(tsel_pending_nonlocal_exit(env)) {
    return tsel_Qnil;
  }
  bool count_named = nargs > 2 && tsel_named_nodes(env, args[2]);
  TSNode child;
  if(count_named) {
    child = ts_node_first_named_child_for_byte(node->node, byte);
  }
  else {
    child = ts_node_first_child_for_byte(node->node, byte);
  }
  TSElNode *wrapped = tsel_node_wrap(child, node->tree);
  if(!wrapped) {
    tsel_signal_error(env, "Allocation failed.");
    return tsel_Qnil;
  }
  return tsel_node_emacs_move(env, wrapped);
}

static const char *tsel_node_descendant_for_byte_range_doc = "Return descendant of NODE for byte range START to END.\n"
  "If TYPE is nil, t, or unspecified include all siblings. Otherwise, if\n"
  "TYPE is the symbol 'named include only named siblings.\n"
  "The behavior of other values for TYPE is unspecified and may change.\n"
  "\n"
  "(fn NODE START END &optional TYPE)";
static emacs_value tsel_node_descendant_for_byte_range(emacs_env *env,
                                                       ptrdiff_t nargs,
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
  for(int i = 1; i < 3; i++) {
    if(!tsel_integer_p(env, args[i])) {
      tsel_signal_wrong_type(env, "integerp", args[i]);
      return tsel_Qnil;
    }
  }
  intmax_t byte_start, byte_end;
  TSEL_SUBR_EXTRACT(integer, env, args[1], &byte_start);
  TSEL_SUBR_EXTRACT(integer, env, args[2], &byte_end);
  byte_start--;
  byte_end--;
  if(tsel_pending_nonlocal_exit(env)) {
    return tsel_Qnil;
  }
  bool count_named = nargs > 3 && tsel_named_nodes(env, args[3]);
  TSNode child;
  if(count_named) {
    child = ts_node_named_descendant_for_byte_range(node->node, byte_start, byte_end);
  }
  else {
    child = ts_node_descendant_for_byte_range(node->node, byte_start, byte_end);
  }
  TSElNode *wrapped = tsel_node_wrap(child, node->tree);
  if(!wrapped) {
    tsel_signal_error(env, "Allocation failed.");
    return tsel_Qnil;
  }
  return tsel_node_emacs_move(env, wrapped);
}

static const char *tsel_node_edit_doc = "Mark NODE as edited.\n"
  "\n"
  "(fn NODE START-BYTE OLD-END-BYTE NEW-END-BYTE START-POINT OLD-END-POINT NEW-END-POINT)";
static emacs_value tsel_node_edit(emacs_env *env,
                                  __attribute__((unused)) ptrdiff_t nargs,
                                  emacs_value *args,
                                  __attribute__((unused)) void *data) {
  // Check argument types
  if(!tsel_node_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-node-p", args[0]);
    return tsel_Qnil;
  }
  for(int i = 1; i < 4; i++) {
    if(!tsel_integer_p(env, args[i])) {
      tsel_signal_wrong_type(env, "integerp", args[i]);
      return tsel_Qnil;
    }
  }
  for(int i = 4; i < 7; i++) {
    if(!tsel_point_p(env, args[i])) {
      tsel_signal_wrong_type(env, "tree-sitter-point-p", args[i]);
      return tsel_Qnil;
    }
  }
  // Extract arguments
  TSElNode *node = tsel_node_get_ptr(env, args[0]);
  if(!node) {
    tsel_signal_error(env, "Failed to get node.");
    return tsel_Qnil;
  }
  TSInputEdit edit;
  intmax_t start_byte, old_end_byte, new_end_byte;
  TSEL_SUBR_EXTRACT(integer, env, args[1], &start_byte);
  TSEL_SUBR_EXTRACT(integer, env, args[2], &old_end_byte);
  TSEL_SUBR_EXTRACT(integer, env, args[3], &new_end_byte);
  edit.start_byte = start_byte - 1;
  edit.old_end_byte = old_end_byte - 1;
  edit.new_end_byte = new_end_byte - 1;
  if(!tsel_point_get_values(env, args[4], &edit.start_point.row, &edit.start_point.column) ||
     !tsel_point_get_values(env, args[5], &edit.old_end_point.row, &edit.old_end_point.column) ||
     !tsel_point_get_values(env, args[6], &edit.new_end_point.row, &edit.new_end_point.column)) {
    tsel_signal_error(env, "Failed to extract points.");
  }
  if(tsel_pending_nonlocal_exit(env)) {
    return tsel_Qnil;
  }
  // Signal the edit
  ts_node_edit(&node->node, &edit);
  node->tree->dirty = true;
  return tsel_Qt;
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
  function_result &= tsel_define_function(env, "tree-sitter-node-eq",
                                          &tsel_node_eq, 2, 2,
                                          tsel_node_eq_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-named-p",
                                          &tsel_node_predicate, 1, 1,
                                          tsel_node_is_named_doc, &tsel_node_predicates[0]);
  function_result &= tsel_define_function(env, "tree-sitter-node-missing-p",
                                          &tsel_node_predicate, 1, 1,
                                          tsel_node_is_missing_doc, &tsel_node_predicates[1]);
  function_result &= tsel_define_function(env, "tree-sitter-node-has-changes-p",
                                          &tsel_node_predicate, 1, 1,
                                          tsel_node_has_changes_doc, &tsel_node_predicates[2]);
  function_result &= tsel_define_function(env, "tree-sitter-node-has-error-p",
                                          &tsel_node_predicate, 1, 1,
                                          tsel_node_has_error_doc, &tsel_node_predicates[3]);
  function_result &= tsel_define_function(env, "tree-sitter-node-parent",
                                          &tsel_node_parent, 1, 1,
                                          tsel_node_parent_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-child-count",
                                          &tsel_node_child_count, 1, 2,
                                          tsel_node_child_count_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-child",
                                          &tsel_node_child, 2, 3,
                                          tsel_node_child_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-next-sibling",
                                          &tsel_node_next_sibling, 1, 2,
                                          tsel_node_next_sibling_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-prev-sibling",
                                          &tsel_node_prev_sibling, 1, 2,
                                          tsel_node_prev_sibling_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-first-child-for-byte",
                                          &tsel_node_first_child_for_byte, 2, 3,
                                          tsel_node_first_child_for_byte_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-descendant-for-byte-range",
                                          &tsel_node_descendant_for_byte_range, 3, 4,
                                          tsel_node_descendant_for_byte_range_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-node-edit",
                                          &tsel_node_edit, 7, 7,
                                          tsel_node_edit_doc, NULL);
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
  if(ts_node_is_null(tree->node)) {
    tsel_node_fin(tree);
    return tsel_Qnil;
  }
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

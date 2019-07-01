/*
 * Copyright (C) 2018, 2019 Karl Otness
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
#include "tree.h"
#include "common.h"
#include "node.h"
#include "point.h"
#include "range.h"

static void tsel_tree_fin(void *ptr) {
  TSElTree *tree = ptr;
  tsel_tree_release(tree);
}

static const char *tsel_tree_p_wrapped_doc = "Return t if OBJECT is a tree-sitter-tree.\n"
  "\n"
  "(fn OBJECT)";
static emacs_value tsel_tree_p_wrapped(emacs_env *env,
                                       __attribute__((unused)) ptrdiff_t nargs,
                                       emacs_value *args,
                                       __attribute__((unused)) void *data) {
  if(tsel_tree_p(env, args[0])) {
    return tsel_Qt;
  }
  return tsel_Qnil;
}

static const char *tsel_tree_root_node_doc = "Return the root node of TREE.\n"
  "\n"
  "(fn TREE)";
static emacs_value tsel_tree_root_node(emacs_env *env,
                                       __attribute__((unused)) ptrdiff_t nargs,
                                       emacs_value *args,
                                       __attribute__((unused)) void *data) {
  TSElTree *tree;
  TSEL_SUBR_EXTRACT(tree, env, args[0], &tree);
  TSNode root = ts_tree_root_node(tree->tree);
  return tsel_node_emacs_move(env, root, tree);
}

static const char *tsel_tree_copy_doc = "Return a shallow copy of TREE.\n"
  "\n"
  "(fn TREE)";
static emacs_value tsel_tree_copy(emacs_env *env,
                                       __attribute__((unused)) ptrdiff_t nargs,
                                       emacs_value *args,
                                       __attribute__((unused)) void *data) {
  TSElTree *tree;
  TSEL_SUBR_EXTRACT(tree, env, args[0], &tree);
  TSTree *new_tree = ts_tree_copy(tree->tree);
  return tsel_tree_emacs_move(env, new_tree);
}

static const char *tsel_tree_edit_doc = "Mark a portion of TREE as edited.\n"
  "\n"
  "(fn TREE START-BYTE OLD-END-BYTE NEW-END-BYTE START-POINT OLD-END-POINT NEW-END-POINT)";
static emacs_value tsel_tree_edit(emacs_env *env,
                                  __attribute__((unused)) ptrdiff_t nargs,
                                  emacs_value *args,
                                  __attribute__((unused)) void *data) {
  TSInputEdit edit;
  TSElTree *tree;
  intmax_t start_byte, old_end_byte, new_end_byte;
  TSEL_SUBR_EXTRACT(tree, env, args[0], &tree);
  TSEL_SUBR_EXTRACT(integer, env, args[1], &start_byte);
  TSEL_SUBR_EXTRACT(integer, env, args[2], &old_end_byte);
  TSEL_SUBR_EXTRACT(integer, env, args[3], &new_end_byte);
  edit.start_byte = start_byte - 1;
  edit.old_end_byte = old_end_byte - 1;
  edit.new_end_byte = new_end_byte - 1;
  TSEL_SUBR_EXTRACT(point, env, args[4], &edit.start_point);
  TSEL_SUBR_EXTRACT(point, env, args[5], &edit.old_end_point);
  TSEL_SUBR_EXTRACT(point, env, args[6], &edit.new_end_point);
  // Signal the edit
  ts_tree_edit(tree->tree, &edit);
  tree->dirty = true;
  return tsel_Qt;
}

static const char *tsel_tree_changed_ranges_doc = "Return a list of changed ranges between TREE-A and TREE-B.\n"
  "\n"
  "(fn TREE-A TREE-B)";
static emacs_value tsel_tree_changed_ranges(emacs_env *env,
                                            __attribute__((unused)) ptrdiff_t nargs,
                                            emacs_value *args,
                                            __attribute__((unused)) void *data) {
  TSElTree *tree_a, *tree_b;
  TSEL_SUBR_EXTRACT(tree, env, args[0], &tree_a);
  TSEL_SUBR_EXTRACT(tree, env, args[1], &tree_b);

  uint32_t count = 0;
  TSRange *ptr = ts_tree_get_changed_ranges(tree_a->tree, tree_b->tree, &count);
  if(count == 0) {
    return tsel_Qnil;
  }
  if(!ptr) {
    tsel_signal_error(env, "Error getting ranges.");
    return tsel_Qnil;
  }
  emacs_value Qcons = env->intern(env, "cons");
  emacs_value list = tsel_Qnil;
  for(size_t i = 0; i < count; i++) {
    TSRange *range = &ptr[count - i - 1];
    emacs_value args[2];
    args[0] = tsel_range_emacs_move(env, range);
    args[1] = list;
    list = env->funcall(env, Qcons, 2, args);
    if(tsel_pending_nonlocal_exit(env)) {
      return tsel_Qnil;
    }
  }
  return list;
}

bool tsel_tree_init(emacs_env *env) {
  bool function_result = tsel_define_function(env, "tree-sitter-tree-p",
                                              &tsel_tree_p_wrapped, 1, 1,
                                              tsel_tree_p_wrapped_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-tree-root-node",
                                          &tsel_tree_root_node, 1, 1,
                                          tsel_tree_root_node_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-tree-copy",
                                          &tsel_tree_copy, 1, 1,
                                          tsel_tree_copy_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-tree-edit",
                                          &tsel_tree_edit, 7, 7,
                                          tsel_tree_edit_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-tree-changed-ranges",
                                          &tsel_tree_changed_ranges, 2, 2,
                                          tsel_tree_changed_ranges_doc, NULL);
  return function_result;
}

emacs_value tsel_tree_emacs_move(emacs_env *env, TSTree *tree) {
  if(!tree) {
    return tsel_Qnil;
  }
  TSElTree *wrapper = malloc(sizeof(TSElTree));
  if(!wrapper) {
    ts_tree_delete(tree);
    tsel_signal_error(env, "Failed to allocate tree.");
    return tsel_Qnil;
  }
  wrapper->refcount = 1;
  wrapper->tree = tree;
  wrapper->dirty = false;
  emacs_value Qts_tree_create = env->intern(env, "tree-sitter-tree--create");
  emacs_value user_ptr = env->make_user_ptr(env, &tsel_tree_fin, wrapper);
  emacs_value func_args[1] = { user_ptr };
  return env->funcall(env, Qts_tree_create, 1, func_args);
}

void tsel_tree_retain(TSElTree *tree) {
  if(!tree) {
    return;
  }
  tree->refcount++;
}

void tsel_tree_release(TSElTree *tree) {
  if(!tree) {
    return;
  }
  if(tree->refcount > 0) {
    tree->refcount--;
  }
  if(tree->refcount == 0) {
    // Time to drop the tree
    if(tree->tree) {
      ts_tree_delete(tree->tree);
    }
    free(tree);
  }
}

bool tsel_tree_p(emacs_env *env, emacs_value obj) {
  if(!tsel_check_record_type(env, "tree-sitter-tree", obj, 1)) {
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
  return !tsel_pending_nonlocal_exit(env) && fin == &tsel_tree_fin;
}

bool tsel_extract_tree(emacs_env *env, emacs_value obj, TSElTree **tree) {
  if(!tsel_tree_p(env, obj)) {
    tsel_signal_wrong_type(env, "tree-sitter-tree-p", obj);
    return false;
  }
  // Get the ptr field
  emacs_value user_ptr;
  if(!tsel_record_get_field(env, obj, 1, &user_ptr)) {
    return false;
  }
  // Get the raw pointer
  TSElTree *ptr = env->get_user_ptr(env, user_ptr);
  if(tsel_pending_nonlocal_exit(env)) {
    return false;
  }
  *tree = ptr;
  return true;
}

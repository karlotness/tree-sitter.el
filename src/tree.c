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
#include "tree.h"
#include "common.h"
#include "node.h"
#include "point.h"

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
  if(!tsel_tree_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-tree-p", args[0]);
    return tsel_Qnil;
  }
  TSElTree *tree = tsel_tree_get_ptr(env, args[0]);
  if(!tree) {
    tsel_signal_error(env, "Failed to get tree.");
    return tsel_Qnil;
  }
  TSNode root = ts_tree_root_node(tree->tree);
  TSElNode *wrapped = tsel_node_wrap(root, tree);
  if(!wrapped) {
    tsel_signal_error(env, "Allocation failed.");
    return tsel_Qnil;
  }
  emacs_value res = tsel_node_emacs_move(env, wrapped);
  if(tsel_pending_nonlocal_exit(env)) {
    tsel_signal_error(env, "Allocation failed.");
    return tsel_Qnil;
  }
  return res;
}

static const char *tsel_tree_copy_doc = "Return a shallow copy of TREE.\n"
  "\n"
  "(fn TREE)";
static emacs_value tsel_tree_copy(emacs_env *env,
                                       __attribute__((unused)) ptrdiff_t nargs,
                                       emacs_value *args,
                                       __attribute__((unused)) void *data) {
  if(!tsel_tree_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-tree-p", args[0]);
    return tsel_Qnil;
  }
  TSElTree *tree = tsel_tree_get_ptr(env, args[0]);
  if(!tree) {
    tsel_signal_error(env, "Failed to get tree.");
    return tsel_Qnil;
  }
  TSTree *new_tree = ts_tree_copy(tree->tree);
  TSElTree *wrapped_tree = tsel_tree_wrap(new_tree);
  emacs_value emacs_tree = tsel_tree_emacs_move(env, wrapped_tree);
  if(!wrapped_tree || tsel_pending_nonlocal_exit(env)) {
    tsel_tree_release(wrapped_tree);
    tsel_signal_error(env, "Failed to initialize new tree");
    return tsel_Qnil;
  }
  return emacs_tree;
}

static const char *tsel_tree_edit_doc = "Mark a portion of TREE as edited.\n"
  "\n"
  "(fn TREE START-BYTE OLD-END-BYTE NEW-END-BYTE START-POINT OLD-END-POINT NEW-END-POINT)";
static emacs_value tsel_tree_edit(emacs_env *env,
                                  __attribute__((unused)) ptrdiff_t nargs,
                                  emacs_value *args,
                                  __attribute__((unused)) void *data) {
  // Check argument types
  if(!tsel_tree_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-tree-p", args[0]);
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
  TSElTree *tree = tsel_tree_get_ptr(env, args[0]);
  if(!tree) {
    tsel_signal_error(env, "Failed to get tree.");
    return tsel_Qnil;
  }
  TSInputEdit edit;
  edit.start_byte = env->extract_integer(env, args[1]) - 1;
  edit.old_end_byte = env->extract_integer(env, args[2]) - 1;
  edit.new_end_byte = env->extract_integer(env, args[3]) - 1;
  if(!tsel_point_get_values(env, args[4], &edit.start_point.row, &edit.start_point.column) ||
     !tsel_point_get_values(env, args[5], &edit.old_end_point.row, &edit.old_end_point.column) ||
     !tsel_point_get_values(env, args[6], &edit.new_end_point.row, &edit.new_end_point.column)) {
    tsel_signal_error(env, "Failed to extract points.");
  }
  if(tsel_pending_nonlocal_exit(env)) {
    return tsel_Qnil;
  }
  // Signal the edit
  ts_tree_edit(tree->tree, &edit);
  tree->dirty = true;
  return tsel_Qt;
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
  return function_result;
}

TSElTree *tsel_tree_wrap(TSTree *tree) {
  TSElTree *wrapper = malloc(sizeof(TSElTree));
  if(!wrapper) {
    return NULL;
  }
  wrapper->refcount = 1;
  wrapper->tree = tree;
  wrapper->dirty = false;
  return wrapper;
}

emacs_value tsel_tree_emacs_move(emacs_env *env, TSElTree *tree) {
  emacs_value Qts_language_create = env->intern(env, "tree-sitter-tree--create");
  emacs_value user_ptr = env->make_user_ptr(env, &tsel_tree_fin, tree);
  emacs_value func_args[1] = { user_ptr };
  return env->funcall(env, Qts_language_create, 1, func_args);
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
    ts_tree_delete(tree->tree);
    free(tree);
  }
}

bool tsel_tree_p(emacs_env *env, emacs_value obj) {
  if(!tsel_check_record_type(env, "tree-sitter-tree", obj)) {
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

TSElTree *tsel_tree_get_ptr(emacs_env *env, emacs_value obj) {
  if(!tsel_tree_p(env, obj)) {
    return NULL;
  }
  // Get the ptr field
  emacs_value user_ptr;
  if(!tsel_record_get_field(env, obj, 1, &user_ptr)) {
    return NULL;
  }
  // Get the raw pointer
  TSElTree *ptr = env->get_user_ptr(env, user_ptr);
  if(tsel_pending_nonlocal_exit(env)) {
    return NULL;
  }
  return ptr;
}

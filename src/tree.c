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

static void tsel_tree_fin(void *ptr) {
  TSElTree *tree = ptr;
  tsel_tree_release(tree);
}

char *tsel_tree_p_wrapped_doc = "Return t if OBJECT is a tree-sitter-tree.\n"
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

bool tsel_tree_init(emacs_env *env) {
  bool function_result = tsel_define_function(env, "tree-sitter-tree-p",
                                              &tsel_tree_p_wrapped, 1, 1,
                                              tsel_tree_p_wrapped_doc, NULL);
  return function_result;
}

TSElTree *tsel_tree_wrap(TSTree *tree) {
  TSElTree *wrapper = malloc(sizeof(TSElTree));
  if(!wrapper) {
    return NULL;
  }
  wrapper->refcount = 1;
  wrapper->tree = tree;
  return wrapper;
}

emacs_value tsel_tree_emacs_move(emacs_env *env, TSElTree *tree) {
  emacs_value Qts_language_create = env->intern(env, "tree-sitter-tree--create");
  emacs_value user_ptr = env->make_user_ptr(env, &tsel_tree_fin, tree);
  emacs_value func_args[1] = { user_ptr };
  return env->funcall(env, Qts_language_create, 1, func_args);
}

void tsel_tree_retain(TSElTree *tree) {
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

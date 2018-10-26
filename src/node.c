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
#include "node.h"

static void tsel_node_fin(void *ptr) {
  TSElNode *node = ptr;
  tsel_node_free(node);
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

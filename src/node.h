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
#pragma once
#ifndef TSEL_NODE_H
#define TSEL_NODE_H
#include <stdbool.h>
#include <emacs-module.h>
#include <tree_sitter/runtime.h>
#include "tree.h"

typedef struct TSElNode {
  TSNode node;
  TSElTree *tree;
} TSElNode;

bool tsel_node_init(emacs_env *env);
TSElNode *tsel_node_wrap(TSNode node, TSElTree *tree);
void tsel_node_free(TSElNode *node);
emacs_value tsel_node_emacs_move(emacs_env *env, TSElNode *node);
bool tsel_node_p(emacs_env *env, emacs_value obj);
TSElNode *tsel_node_get_ptr(emacs_env *env, emacs_value obj);
bool tsel_extract_node(emacs_env *env, emacs_value obj, TSElNode **node);

#endif //ifndef TSEL_NODE_H

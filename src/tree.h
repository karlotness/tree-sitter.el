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
#ifndef TSEL_TREE_H
#define TSEL_TREE_H
#include <stdbool.h>
#include <emacs-module.h>
#include <tree_sitter/runtime.h>

typedef struct TSElTree {
  uintptr_t refcount;
  TSTree *tree;
} TSElTree;

bool tsel_tree_init(emacs_env *env);
TSElTree *tsel_tree_wrap(TSTree *tree);
emacs_value tsel_tree_emacs_move(emacs_env *env, TSElTree *tree);
void tsel_tree_retain(TSElTree *tree);
void tsel_tree_release(TSElTree *tree);
bool tsel_tree_p(emacs_env *env, emacs_value obj);
TSElTree *tsel_tree_get_ptr(emacs_env *env, emacs_value obj);

#endif //ifndef TSEL_TREE_H

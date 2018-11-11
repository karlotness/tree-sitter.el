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
#include <emacs-module.h>
#include "common.h"
#include "language.h"
#include "symbol.h"
#include "parser.h"
#include "tree.h"
#include "node.h"
#include "point.h"
#include "range.h"

// Required symbol for Emacs loading
int plugin_is_GPL_compatible;

// Bind to Emacs
int emacs_module_init(struct emacs_runtime *ert) {
  emacs_env *env = ert->get_environment(ert);
  // Require the definitions
  emacs_value require_feature = env->intern(env, "tree-sitter-defs");
  emacs_value Qrequire = env->intern(env, "require");
  emacs_value args[1] = { require_feature };
  env->funcall(env, Qrequire, 1, args);
  if(tsel_pending_nonlocal_exit(env)) {
    return 3;
  }
  // Perform initialization
  if(!tsel_common_init(env) || !tsel_language_init(env) ||
     !tsel_symbol_init(env) || !tsel_parser_init(env) ||
     !tsel_tree_init(env) || !tsel_node_init(env) ||
     !tsel_point_init(env) || !tsel_range_init(env)) {
    return 1;
  }
  // Provide the module
  emacs_value feature_name = env->intern(env, "tree-sitter-module");
  emacs_value provide_symbol = env->intern(env, "provide");
  args[0] = feature_name;
  env->funcall(env, provide_symbol, 1, args);
  if(tsel_pending_nonlocal_exit(env)) {
    return 4;
  }
  return 0;
}

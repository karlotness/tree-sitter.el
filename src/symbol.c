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
#include "symbol.h"
#include "common.h"

bool tsel_symbol_init(__attribute__((unused)) emacs_env *env) {
  return true;
}

bool tsel_symbol_p(emacs_env *env, emacs_value obj) {
  // Ensure the pointer is wrapped in the proper Emacs structure type
  emacs_value Qts_lang_p = env->intern(env, "tree-sitter-symbol-p");
  emacs_value args[1] = { obj };
  if(!env->eq(env, env->funcall(env, Qts_lang_p, 1, args), tsel_Qt) ||
     tsel_pending_nonlocal_exit(env)) {
    return false;
  }
  // Extract the "code" field and make sure it's an integer
  emacs_value Qts_symbol_code = env->intern(env, "tree-sitter-symbol-code");
  emacs_value code = env->funcall(env, Qts_symbol_code, 1, args);
  emacs_value Qintegerp = env->intern(env, "integerp");
  args[0] = code;
  if(!env->eq(env, env->funcall(env, Qintegerp, 1, args), tsel_Qt) ||
     tsel_pending_nonlocal_exit(env)) {
    return false;
  }
  intmax_t raw_code = env->extract_integer(env, code);
  return raw_code >= 0;
}

bool tsel_symbol_get_code(emacs_env *env, emacs_value obj, uint16_t *code_out) {
  if(!tsel_symbol_p(env, obj)) {
    return false;
  }
  // Extract the "code" field and make sure it's an integer
  emacs_value Qts_symbol_code = env->intern(env, "tree-sitter-symbol-code");
  emacs_value args[1] = { obj };
  emacs_value code = env->funcall(env, Qts_symbol_code, 1, args);
  uint16_t raw_code = env->extract_integer(env, code);
  if(tsel_pending_nonlocal_exit(env)) {
    return false;
  }
  *code_out = raw_code;
  return true;
}

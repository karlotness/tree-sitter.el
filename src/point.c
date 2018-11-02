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
#include "point.h"
#include "common.h"

static char *tsel_point_p_wrapped_doc = "Return t if OBJECT is a tree-sitter-point.\n"
  "\n"
  "(fn OBJECT)";
static emacs_value tsel_point_p_wrapped(emacs_env *env,
                                       __attribute__((unused)) ptrdiff_t nargs,
                                       emacs_value *args,
                                       __attribute__((unused)) void *data) {
  if(tsel_point_p(env, args[0])) {
    return tsel_Qt;
  }
  return tsel_Qnil;
}

bool tsel_point_init(emacs_env *env) {
  bool function_result = tsel_define_function(env, "tree-sitter-point-p",
                                              &tsel_point_p_wrapped, 1, 1,
                                              tsel_point_p_wrapped_doc, NULL);
  return function_result;
}

bool tsel_point_p(emacs_env *env, emacs_value obj) {
  if(!tsel_check_record_type(env, "tree-sitter-point", obj)) {
    return false;
  }
  emacs_value Qintegerp = env->intern(env, "integerp");
  // Check that both fields are integers
  emacs_value val;
  if(!tsel_record_get_field(env, obj, 1, &val) ||
     !env->eq(env, env->funcall(env, Qintegerp, 1, &val), tsel_Qt) ||
     !tsel_record_get_field(env, obj, 2, &val) ||
     !env->eq(env, env->funcall(env, Qintegerp, 1, &val), tsel_Qt) ||
     tsel_pending_nonlocal_exit(env)) {
    return false;
  }
  return true;
}

bool tsel_point_get_values(emacs_env *env, emacs_value point, uint32_t *row, uint32_t *col) {
  if(!tsel_point_p(env, point)) {
    return false;
  }
  emacs_value val;
  uint32_t num;
  uint32_t *locs[2] = {row, col};
  for(int i = 0; i < 2; i++) {
    // Skip any values which aren't requested
    if(!locs[i]) {
      continue;
    }
    // Get the field
    if(!tsel_record_get_field(env, point, i, &val)) {
      return false;
    }
    num = env->extract_integer(env, val);
    if(tsel_pending_nonlocal_exit(env)) {
      return false;
    }
    if(i == 0) {
      // Correct for fact that Emacs rows are indexed from 1
      num--;
    }
    *locs[i] = num;
  }
  return true;
}

emacs_value tsel_point_emacs_move(emacs_env *env, const TSPoint *point) {
  emacs_value Qts_point_create = env->intern(env, "tree-sitter-point--create");
  emacs_value args[2];
  args[0] = env->make_integer(env, point->row + 1);
  args[1] = env->make_integer(env, point->column);
  return env->funcall(env, Qts_point_create, 2, args);
}

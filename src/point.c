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

static const char *tsel_point_p_wrapped_doc = "Return t if OBJECT is a tree-sitter-point.\n"
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
  if(!tsel_check_record_type(env, "tree-sitter-point", obj, 2)) {
    return false;
  }
  // Check that both fields are integers
  emacs_value val;
  if(!tsel_record_get_field(env, obj, 1, &val) ||
     !tsel_integer_p(env, val) ||
     !tsel_record_get_field(env, obj, 2, &val) ||
     !tsel_integer_p(env, val)) {
    return false;
  }
  return true;
}

bool tsel_extract_point(emacs_env *env, emacs_value obj, TSPoint *point) {
  if(!tsel_point_p(env, obj)) {
    tsel_signal_wrong_type(env, "tree-sitter-point-p", obj);
    return false;
  }
  for(int i = 0; i < 2; i++) {
    // Get the field
    emacs_value val;
    if(!tsel_record_get_field(env, obj, i + 1, &val)) {
      return false;
    }
    intmax_t num = env->extract_integer(env, val);
    if(tsel_pending_nonlocal_exit(env)) {
      return false;
    }
    if(i == 0) {
      // Correct for fact that Emacs rows are indexed from 1
      point->row = num - 1;
    }
    else {
      point->column = num;
    }
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

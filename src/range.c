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
#include "range.h"
#include "common.h"
#include "point.h"

static const char *tsel_range_p_wrapped_doc = "Return t if OBJECT is a tree-sitter-range.\n"
  "\n"
  "(fn OBJECT)";
static emacs_value tsel_range_p_wrapped(emacs_env *env,
                                        __attribute__((unused)) ptrdiff_t nargs,
                                        emacs_value *args,
                                        __attribute__((unused)) void *data) {
  if(tsel_range_p(env, args[0])) {
    return tsel_Qt;
  }
  return tsel_Qnil;
}

bool tsel_range_init(emacs_env *env) {
  bool function_result = tsel_define_function(env, "tree-sitter-range-p",
                                              &tsel_range_p_wrapped, 1, 1,
                                              tsel_range_p_wrapped_doc, NULL);
  return function_result;
}

bool tsel_range_p(emacs_env *env, emacs_value obj) {
  if(!tsel_check_record_type(env, "tree-sitter-range", obj, 4)) {
    return false;
  }
  // Check that first two fields are points
  for(int i = 1; i < 3; i++) {
    emacs_value field;
    if(!tsel_record_get_field(env, obj, i, &field) ||
       !tsel_point_p(env, field)) {
      return false;
    }
  }
  // Check that last two fields are integers
  for(int i = 3; i < 5; i++) {
    emacs_value field;
    if(!tsel_record_get_field(env, obj, i, &field) ||
       !tsel_integer_p(env, field)) {
      return false;
    }
  }
  return true;
}

bool tsel_extract_range(emacs_env *env, emacs_value obj, TSRange *point) {
  if(!tsel_range_p(env, obj)) {
    tsel_signal_wrong_type(env, "tree-sitter-point-p", obj);
    return false;
  }
  // Extract values
  emacs_value val;
  intmax_t start_byte, end_byte;
  if(!tsel_record_get_field(env, obj, 1, &val) ||
     !tsel_extract_point(env, val, &point->start_point) ||
     !tsel_record_get_field(env, obj, 2, &val) ||
     !tsel_extract_point(env, val, &point->end_point) ||
     !tsel_record_get_field(env, obj, 3, &val) ||
     !tsel_extract_integer(env, obj, &start_byte) ||
     !tsel_record_get_field(env, obj, 4, &val) ||
     !tsel_extract_integer(env, obj, &end_byte)) {
    return false;
  }
  if(tsel_pending_nonlocal_exit(env)) {
    return false;
  }
  point->start_byte = start_byte - 1;
  point->end_byte = end_byte - 1;
  return true;
}

emacs_value tsel_range_emacs_move(emacs_env *env, const TSRange *point) {
  emacs_value Qtsel_range_create = env->intern(env, "tree-sitter-range--create");
  emacs_value args[4];
  args[0] = tsel_point_emacs_move(env, &point->start_point);
  args[1] = tsel_point_emacs_move(env, &point->end_point);
  args[2] = env->make_integer(env, point->start_byte + 1);
  args[3] = env->make_integer(env, point->end_byte + 1);
  return env->funcall(env, Qtsel_range_create, 4, args);
}

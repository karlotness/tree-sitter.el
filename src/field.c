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
#include "field.h"
#include "common.h"

static const char *tsel_field_p_wrapped_doc = "Return t if OBJECT is a tree-sitter-field.\n"
  "\n"
  "(fn OBJECT)";
static emacs_value tsel_field_p_wrapped(emacs_env *env,
                                         __attribute__((unused)) ptrdiff_t nargs,
                                         emacs_value *args,
                                         __attribute__((unused)) void *data) {
  if(tsel_field_p(env, args[0])) {
    return tsel_Qt;
  }
  return tsel_Qnil;
}

bool tsel_field_init(__attribute__((unused)) emacs_env *env) {
  bool function_result = tsel_define_function(env, "tree-sitter-field-p",
                                              &tsel_field_p_wrapped, 1, 1,
                                              tsel_field_p_wrapped_doc, NULL);
  return function_result;
}

bool tsel_field_p(emacs_env *env, emacs_value obj) {
  if(!tsel_check_record_type(env, "tree-sitter-field", obj, 1)) {
    return false;
  }
  // Get the code field
  emacs_value code;
  if(!tsel_record_get_field(env, obj, 1, &code)) {
    return false;
  }
  // Ensure the code is non-negative
  intmax_t raw_code = env->extract_integer(env, code);
  if(raw_code <= 0 || tsel_pending_nonlocal_exit(env)){
    env->non_local_exit_clear(env);
    return false;
  }
  return true;
}

bool tsel_extract_tsfieldid(emacs_env *env, emacs_value obj, TSFieldId *code_out) {
  if(!tsel_field_p(env, obj)) {
    tsel_signal_wrong_type(env, "tree-sitter-field-p", obj);
    return false;
  }
  // Extract the "code" field
  emacs_value code;
  if(!tsel_record_get_field(env, obj, 1, &code)) {
    return false;
  }
  // Convert the code to an integer
  intmax_t raw_code = env->extract_integer(env, code);
  if(tsel_pending_nonlocal_exit(env)){
    return false;
  }
  *code_out = raw_code;
  return true;
}

emacs_value tsel_field_emacs_move(emacs_env *env, TSFieldId code) {
  emacs_value ecode = env->make_integer(env, code);
  emacs_value Qts_symb_create = env->intern(env, "tree-sitter-field--create");
  emacs_value args[1] = { ecode };
  return env->funcall(env, Qts_symb_create, 1, args);
}

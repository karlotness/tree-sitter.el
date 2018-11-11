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
#ifndef TSEL_COMMON_H
#define TSEL_COMMON_H
#include <emacs-module.h>
#include <stdbool.h>

#define TSEL_SUBR_EXTRACT(type, env, obj, res)                          \
  if(!tsel_extract_##type(env, obj, res)) {                             \
    if(tsel_pending_nonlocal_exit(env)) {                               \
      return tsel_Qnil;                                                 \
    }                                                                   \
    tsel_signal_error(env, "Failed to extract " #type ".");             \
    return tsel_Qnil;                                                   \
  }

typedef emacs_value (emacs_function) (emacs_env *env,
                                      ptrdiff_t nargs, emacs_value *args,
                                      void *data);
typedef void (emacs_finalizer) (void *obj);

bool tsel_common_init(emacs_env *env);
bool tsel_pending_nonlocal_exit(emacs_env *env);
void tsel_signal_wrong_type(emacs_env *env, char *type_pred_name, emacs_value val_provided);
bool tsel_define_function(emacs_env *env, char *function_name, emacs_function *func,
                          ptrdiff_t min_arg_count, ptrdiff_t max_arg_count, const char *doc,
                          void *data);
bool tsel_record_get_field(emacs_env *env, emacs_value obj, uint8_t field, emacs_value *out);
bool tsel_check_record_type(emacs_env *env, char *record_type, emacs_value obj);
bool tsel_string_p(emacs_env *env, emacs_value obj);
bool tsel_extract_string(emacs_env *env, emacs_value obj, char **res);
void tsel_signal_error(emacs_env *env, char *message);
bool tsel_integer_p(emacs_env *env, emacs_value obj);
bool tsel_extract_integer(emacs_env *env, emacs_value obj, intmax_t *res);
bool tsel_extract_buffer(emacs_env *env, emacs_value obj, emacs_value *res);

extern emacs_value tsel_Qnil;
extern emacs_value tsel_Qt;

#endif //ifndef TSEL_COMMON_H

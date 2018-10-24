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
#include "common.h"

emacs_value tsel_Qnil;
emacs_value tsel_Qt;

bool tsel_init_common(emacs_env *env) {
  tsel_Qt = env->make_global_ref(env, env->intern(env, "t"));
  tsel_Qnil = env->make_global_ref(env, env->intern(env, "nil"));
  if(tsel_pending_nonlocal_exit(env)) {
    return false;
  }
  return true;
}

bool tsel_pending_nonlocal_exit(emacs_env *env) {
  return env->non_local_exit_check(env) != emacs_funcall_exit_return;
}

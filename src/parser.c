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
#include "parser.h"
#include "common.h"

static void tsel_parser_fin(void *ptr) {
  ts_parser_delete(ptr);
}

char *tsel_parser_new_doc = "Create a new tree-sitter parser.\n";
static emacs_value tsel_parser_new(emacs_env *env,
                                           __attribute__((unused)) ptrdiff_t nargs,
                                           __attribute__((unused)) emacs_value *args,
                                           __attribute__((unused)) void *data) {
  TSParser *parser = ts_parser_new();
  emacs_value new_parser = env->make_user_ptr(env, &tsel_parser_fin, parser);
  emacs_value Qts_parser_create = env->intern(env, "tree-sitter-parser--create");
  emacs_value funargs[1] = { new_parser };
  emacs_value res = env->funcall(env, Qts_parser_create, 1, funargs);
  if(tsel_pending_nonlocal_exit(env)) {
    ts_parser_delete(parser);
    return tsel_Qnil;
  }
  return res;
}

bool tsel_parser_init(emacs_env *env) {
  bool function_result = tsel_define_function(env, "tree-sitter-parser-new",
                                              &tsel_parser_new, 0, 0,
                                              tsel_parser_new_doc, NULL);
  return function_result;
}

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
#include <stdlib.h>
#include <string.h>
#include "parser.h"
#include "common.h"
#include "tree.h"

#define TSEL_PARSE_CHAR_BUFFER_SIZE 500
static char tsel_parser_char_buffer[TSEL_PARSE_CHAR_BUFFER_SIZE + 1] = {0};
static emacs_value Qts_buffer_substring;
static emacs_value emacs_buffer_read_length;

static void tsel_parser_fin(void *ptr) {
  TSElParser *parser = ptr;
  ts_parser_delete(parser->parser);
  free(parser);
}

static const char *tsel_parser_new_doc = "Create a new tree-sitter parser.\n";
static emacs_value tsel_parser_new(emacs_env *env,
                                   __attribute__((unused)) ptrdiff_t nargs,
                                   __attribute__((unused)) emacs_value *args,
                                   __attribute__((unused)) void *data) {
  TSElParser *wrapper = malloc(sizeof(TSElParser));
  TSParser *parser = ts_parser_new();
  if(!wrapper || !parser) {
    if(wrapper) {
      free(wrapper);
    }
    if(parser) {
      ts_parser_delete(parser);
    }
    tsel_signal_error(env, "Initialization failed");
    return tsel_Qnil;
  }
  wrapper->parser = parser;
  wrapper->lang = NULL;
  emacs_value new_parser = env->make_user_ptr(env, &tsel_parser_fin, wrapper);
  emacs_value Qts_parser_create = env->intern(env, "tree-sitter-parser--create");
  emacs_value funargs[1] = { new_parser };
  emacs_value res = env->funcall(env, Qts_parser_create, 1, funargs);
  if(tsel_pending_nonlocal_exit(env)) {
    tsel_parser_fin(wrapper);
    tsel_signal_error(env, "Initialization failed");
    return tsel_Qnil;
  }
  return res;
}

static const char *tsel_parser_p_wrapped_doc = "Return t if OBJECT is a tree-sitter-parser.\n"
  "\n"
  "(fn OBJECT)";
static emacs_value tsel_parser_p_wrapped(emacs_env *env,
                                         __attribute__((unused)) ptrdiff_t nargs,
                                         emacs_value *args,
                                         __attribute__((unused)) void *data) {
  if(tsel_parser_p(env, args[0])) {
    return tsel_Qt;
  }
  return tsel_Qnil;
}

static const char *tsel_parser_language_doc = "Return the language of parser PARSE.\n"
  "\n"
  "(fn PARSE)";
static emacs_value tsel_parser_language(emacs_env *env,
                                        __attribute__((unused)) ptrdiff_t nargs,
                                        emacs_value *args,
                                        __attribute__((unused)) void *data) {
  if(!tsel_parser_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-parser-p", args[0]);
    return tsel_Qnil;
  }
  TSElParser *parse = tsel_parser_get_ptr(env, args[0]);
  TSElLanguage *lang = parse->lang;
  if(!lang) {
    return tsel_Qnil;
  }
  return tsel_language_wrap(env, lang);
}

struct tsel_parser_buffer_payload {
  emacs_env *env;
  emacs_value buffer;
};

static const char *tsel_parser_read_buffer_function(void *payload, uint32_t byte_index,
                                                    __attribute__((unused)) TSPoint position,
                                                    uint32_t *bytes_read) {
  // Extract the payload
  struct tsel_parser_buffer_payload *buf_payload = payload;
  emacs_env *env = buf_payload->env;
  emacs_value buffer = buf_payload->buffer;
  // Call our buffer function to get a string
  emacs_value byte_pos = env->make_integer(env, byte_index + 1);
  // Leave one char left over so Emacs doesn't complain about the buffer size
  emacs_value args[3] = { buffer, byte_pos, emacs_buffer_read_length };
  emacs_value str = env->funcall(env, Qts_buffer_substring, 3, args);
  ptrdiff_t size = TSEL_PARSE_CHAR_BUFFER_SIZE;
  if(!env->copy_string_contents(env, str, (char*) &tsel_parser_char_buffer, &size)) {
    *bytes_read = 0;
    return NULL;
  }
  // Add our own null character, just to be sure
  tsel_parser_char_buffer[TSEL_PARSE_CHAR_BUFFER_SIZE] = '\0';
  *bytes_read = size - 1;
  return (char*) &tsel_parser_char_buffer;
}

static const char *tsel_parser_parse_buffer_doc = "Use parser PARSE on buffer BUF.\n"
  "Returns the resulting parse tree.\n"
  "\n"
  "(fn PARSE BUF &optional TREE)";
static emacs_value tsel_parser_parse_buffer(emacs_env *env,
                                            ptrdiff_t nargs,
                                            emacs_value *args,
                                            __attribute__((unused)) void *data) {
  if(!tsel_parser_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-parser-p", args[0]);
    return tsel_Qnil;
  }
  // Check that second arg is a buffer
  emacs_value Qbufferp = env->intern(env, "bufferp");
  if(!env->eq(env, tsel_Qt, env->funcall(env, Qbufferp, 1, &args[1])) ||
     tsel_pending_nonlocal_exit(env)) {
    tsel_signal_wrong_type(env, "bufferp", args[1]);
    return tsel_Qnil;
  }
  TSElTree *tree = NULL;
  // If we have a third arg, make sure it is a tree
  if(nargs >= 3 && !env->eq(env, tsel_Qnil, args[2])) {
    if(!tsel_tree_p(env, args[2]) || tsel_pending_nonlocal_exit(env)) {
      tsel_signal_wrong_type(env, "tree-sitter-tree-p", args[2]);
      return tsel_Qnil;
    }
    else {
      TSElTree *init_tree = tsel_tree_get_ptr(env, args[2]);
      if(init_tree) {
        tree = init_tree;
      }
    }
    if(tsel_pending_nonlocal_exit(env)) {
      tsel_signal_error(env, "Failed to get tree");
      return tsel_Qnil;
    }
  }
  struct tsel_parser_buffer_payload payload = {.env = env,
                                               .buffer = args[1]};
  TSInput input_def = {.payload = &payload,
                       .encoding = TSInputEncodingUTF8,
                       .read = &tsel_parser_read_buffer_function};
  TSParser *parse = tsel_parser_get_ptr(env, args[0])->parser;
  TSTree *new_tree = NULL;
  if(!tree || tree->dirty) {
    // No tree given or tree is dirty
    new_tree = ts_parser_parse(parse, tree ? tree->tree : NULL, input_def);
  }
  else {
    // Tree is specified but not dirty, just make a copy
    new_tree = ts_tree_copy(tree->tree);
  }
  TSElTree *wrapped_tree = tsel_tree_wrap(new_tree);
  emacs_value emacs_tree = tsel_tree_emacs_move(env, wrapped_tree);
  if(!wrapped_tree || tsel_pending_nonlocal_exit(env)) {
    tsel_tree_release(wrapped_tree);
    tsel_signal_error(env, "Failed to initialize new tree");
    return tsel_Qnil;
  }
  return emacs_tree;
}


static const char *tsel_parser_set_language_doc = "Set the language of parser PARSE to LANG.\n"
  "\n"
  "(fn PARSE LANG)";
static emacs_value tsel_parser_set_language(emacs_env *env,
                                            __attribute__((unused)) ptrdiff_t nargs,
                                            emacs_value *args,
                                            __attribute__((unused)) void *data) {
  if(!tsel_parser_p(env, args[0])) {
    tsel_signal_wrong_type(env, "tree-sitter-parser-p", args[0]);
    return tsel_Qnil;
  }
  bool lang_is_nil = env->eq(env, args[1], tsel_Qnil);
  if((!tsel_language_p(env, args[1]) && !lang_is_nil) ||
     tsel_pending_nonlocal_exit(env)) {
    tsel_signal_wrong_type(env, "tree-sitter-language-p", args[1]);
    return tsel_Qnil;
  }
  TSElParser *parse = tsel_parser_get_ptr(env, args[0]);
  TSElLanguage *lang = NULL;
  TSLanguage *raw_lang = NULL;
  if(!lang_is_nil) {
    lang = tsel_language_get_ptr(env, args[1]);
    raw_lang = lang->ptr;
  }
  if(tsel_pending_nonlocal_exit(env) ||
     !ts_parser_set_language(parse->parser, raw_lang)) {
    tsel_signal_error(env, "Failed to set language");
    return tsel_Qnil;
  }
  parse->lang = lang;
  return tsel_Qnil;
}

bool tsel_parser_p(emacs_env *env, emacs_value obj) {
  if(!tsel_check_record_type(env, "tree-sitter-parser", obj)) {
    return false;
  }
  // Get the ptr field
  emacs_value user_ptr;
  if(!tsel_record_get_field(env, obj, 1, &user_ptr)) {
    return false;
  }
  // Make sure it's a user pointer
  emacs_value Quser_ptrp = env->intern(env, "user-ptrp");
  emacs_value args[1] = { user_ptr };
  if(!env->eq(env, env->funcall(env, Quser_ptrp, 1, args), tsel_Qt) ||
     tsel_pending_nonlocal_exit(env)) {
    return false;
  }
  // Check the finalizer
  emacs_finalizer *fin = env->get_user_finalizer(env, user_ptr);
  return !tsel_pending_nonlocal_exit(env) && fin == &tsel_parser_fin;
}

bool tsel_parser_init(emacs_env *env) {
  Qts_buffer_substring = env->make_global_ref(env, env->intern(env, "tree-sitter--buffer-substring"));
  emacs_buffer_read_length = env->make_global_ref(env, env->make_integer(env, TSEL_PARSE_CHAR_BUFFER_SIZE - 1));
  if(tsel_pending_nonlocal_exit(env)) {
    return false;
  }
  bool function_result = tsel_define_function(env, "tree-sitter-parser-new",
                                              &tsel_parser_new, 0, 0,
                                              tsel_parser_new_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-parser-p",
                                          &tsel_parser_p_wrapped, 1, 1,
                                          tsel_parser_p_wrapped_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-parser-language",
                                          &tsel_parser_language, 1, 1,
                                          tsel_parser_language_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-parser-set-language",
                                          &tsel_parser_set_language, 2, 2,
                                          tsel_parser_set_language_doc, NULL);
  function_result &= tsel_define_function(env, "tree-sitter-parser-parse-buffer",
                                          &tsel_parser_parse_buffer, 2, 3,
                                          tsel_parser_parse_buffer_doc, NULL);
  return function_result;
}

TSElParser *tsel_parser_get_ptr(emacs_env *env, emacs_value obj) {
  if(!tsel_parser_p(env, obj)) {
    return NULL;
  }
  // Get the ptr field
  emacs_value user_ptr;
  if(!tsel_record_get_field(env, obj, 1, &user_ptr)) {
    return NULL;
  }
  // Get the raw pointer
  TSElParser *ptr = env->get_user_ptr(env, user_ptr);
  if(tsel_pending_nonlocal_exit(env)) {
    return NULL;
  }
  return ptr;
}

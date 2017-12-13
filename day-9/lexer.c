// http://adventofcode.com/2017/day/9

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Lexer {
  int index;
  char* tokens;
} Lexer;

char* clean_input(char* source) {
  int length = strlen(source);
  char* target = malloc(length * sizeof(char));
  char* writer = target;
  int index = 0;

  for (int index = 0; index < length; index++) {
    char token = source[index];

    if (token == '!') {
      index += 1;
    } else {
      *writer = token;
      writer++;
    }
  }

  return target;
}

Lexer* create_lexer(char* src) {
  Lexer *lexer;
  lexer->index = 0;
  lexer->tokens = clean_input(src);
  return lexer;
}

int has_tokens(Lexer *lexer) {
  return lexer->index < strlen(lexer->tokens);
}

char peek(Lexer *lexer) {
  return lexer->tokens[lexer->index];
}

char take(Lexer *lexer) {
  return lexer->tokens[lexer->index++];
}

char* take_until(Lexer *lexer, char target) {
  int start_index = lexer->index;

  while (has_tokens(lexer) && peek(lexer) != target) {
    take(lexer);
  }

  int end_index = lexer->index;
  int length = end_index - start_index + 1;

  take(lexer);

  char* tokens = malloc(length * sizeof(char));
  strncpy(tokens, lexer->tokens + start_index, length);
  return tokens;
}


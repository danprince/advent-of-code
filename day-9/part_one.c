// http://adventofcode.com/2017/day/9

#include <stdio.h>
#include "lexer.c"

int solve(char* input) {
  Lexer *lexer = create_lexer(input);
  int score = 0;
  int depth = 0;

  while (has_tokens(lexer)) {
    char token = take(lexer);
    if (token == '{') depth += 1;
    if (token == '<') take_until(lexer, '>');
    if (token == '}') score += depth, depth -= 1;
  }

  return score;
}

int main(void) {
  int score = solve("{{<a!>},{<a!>},{<a!>},{<ab>}}");
  printf("%d\n", score);
  return 0;
}

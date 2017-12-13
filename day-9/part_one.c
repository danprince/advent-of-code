// http://adventofcode.com/2017/day/9

#include <stdio.h>
#include <string.h>

int solve(char* input) {
  int in_garbage = 0;
  int score = 0;
  int depth = 0;

  for (int i = 0; i < strlen(input); i++) {
    char token = input[i];

    if (token == '!') i++;
    if (token == '>' && in_garbage) in_garbage = 0;

    if (!in_garbage) {
      if (token == '{') depth += 1;
      if (token == '<') in_garbage = 1;
      if (token == '}') score += depth, depth -= 1;
    }
  }

  return score;
}

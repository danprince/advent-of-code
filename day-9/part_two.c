// http://adventofcode.com/2017/day/9

#include <stdio.h>
#include <string.h>

int solve(char* input) {
  int in_garbage = 0;
  int score = 0;

  for (int i = 0; i < strlen(input); i++) {
    char token = input[i];

    if (token == '!') i++;
    if (token != '>' && token != '!' && in_garbage) score++;
    if (token == '>' && in_garbage) in_garbage = 0;
    if (token == '<') in_garbage = 1;
  }

  return score;
}

// http://adventofcode.com/2017/day/15

let { areGeneratorsMatching } = require("./PartOne");

function* createGenerator(seed, multipleOf, factor) {
  let value = seed;

  while (true) {
    value = (value * factor) % 2147483647;
    if (value % multipleOf === 0) {
      yield value;
    }
  }
}

function solve(a, b) {
  let generatorA = createGenerator(a, 4, 16807);
  let generatorB = createGenerator(b, 8, 48271);
  let matchedPairCount = 0;

  for (let i = 0; i < 5000000; i++) {
    if (areGeneratorsMatching(generatorA, generatorB)) {
      matchedPairCount += 1;
    }
  }

  return matchedPairCount;
}


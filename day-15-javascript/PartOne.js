// http://adventofcode.com/2017/day/15

function* createGenerator(seed, factor) {
  let value = seed;

  while (true) {
    value = (value * factor) % 2147483647;
    yield value;
  }
}

function areGeneratorsMatching(...generators) {
  let current;

  for (let gen of generators) {
    let value = gen.next().value;
    let masked = value & 0xFFFF;
    if (current === undefined) current = masked;
    if (current !== masked) {
      return false;
    }
  }

  return true;
}

function solve(a, b) {
  let generatorA = createGenerator(a, 16807);
  let generatorB = createGenerator(b, 48271);
  let matchedPairCount = 0;

  for (let i = 0; i < 40000000; i++) {
    if (areGeneratorsMatching(generatorA, generatorB)) {
      matchedPairCount += 1;
    }
  }

  return matchedPairCount;
}

module.exports = { areGeneratorsMatching };

This puzzle laid a deliberate trap, and I walked straight into it.

We're observing a growing school of lanternfish, where each individual fish has an internal timer. When the timer ticks down to 0, it spawns a new fish and resets its timer to 6 days. New fish always spawn with an internal timer of 8 days. To complicate matters, the fish are not synchronised, and each one has an individual timer.

The problem was simple enough. Given a starting population, how many lanternfish will there be after 80 days?

```txt
Initial state: 3,4,3,1,2
After  1 day:  2,3,2,0,1
After  2 days: 1,2,1,6,0,8
After  3 days: 0,1,0,5,6,7,8
After  4 days: 6,0,6,4,5,6,7,8,8
...
```

I implemented a solution that I could verify against the examples by representing the population of lanternfish with a sequence, where each item in the sequence was the timer for an individual lanternfish.

Simulating a single unit of time was as simple as iterating over the array, decrementing the timer, and making a note to spawn a new fish at the end.

```nim
proc simulate(fish: var seq[int]) =
  var spawn = 0

  for i in 0 ..< fish.len:
    fish[i] -= 1
    if fish[i] == 0:
      spawn += 1
      fish[i] = 6

  for i in 0 .. spawn:
    fish.add(8)
```

This approach solved 80 iterations no problem, but the sequence had grown to nearly 400,000 elements for my input.

I only spotted the trap when I saw the second part of the puzzle.

> _How many lanternfish would there be after 256 days?_

256 iterations of exponential growth meant that a memory-bound solution wasn't going to cut it here.

My first take was to try and subdivide the problem. I would calculate the size of the final population for each starting fish in my input array individually. Unfortunately, hundreds of iterations of exponential growth also shuts down that idea, too. I could have recursed further inside the array to subdivide this problem, but it start to feel like you're working against yourself.

So I started down the rabbit hole of finding a closed form solution. My intuition for these kinds of mathematical problems isn't great and it didn't seem unreasonable to calculate the rate of change for the population.

My idea was to synchronise all fish initially (keeping a record of the time it took to synchronise them) then use the closed form solution to calculate the size of the population after a certain number of days.

The population will increase at roughly `N/6` fish per day, where `N` is the current number of fish. However, I couldn't think of any sane way to integrate the 8 cycle fish into an equation. Retrospectively, I suspect that's the reason why it was part of the puzzle.

I ended up having to put this down whilst I cracked on with work for the day, then picked it up again afterwards, and the solution seemed quite simple.

Rather than keeping track of every single fish, keep track of how many fish there are for each timer cycle.

```nim
@[3,4,3,1,2]

# becomes

{0: 0,
 1: 1,
 2: 1,
 3: 2,
 4: 1,
 5: 0,
 6: 0,
 7: 0,
 8: 0}.toTable
```

The benefit of this structure is that the table never grows, regardless of how many fish you have, and rather than considering each fish individually, you can update all fish with a given timer value in one go.

Like [yesterday](../day-5), I initially thought it would be a good use for a `CountTable`, but when I tried that I discovered that you can't decrement a key, they only count upwards.

```nim
const NewFishCycle = 8
const OldFishCycle = 6

proc countFishAfterDays(initial: seq[int], days: int): int =
  var population: OrderedTable[int, int]

  for i in 0 .. NewFishCycle:
    population[i] = 0

  for fish in initial:
    population[fish] += 1

  for day in 1 .. days:
    let newFish = population[0]
    for i in 1 .. NewFishCycle:
      population[i - 1] = population[i]
    population[OldFishCycle] += newFish
    population[NewFishCycle] = newFish

  for age, count in population:
    result += count
```

I tripped up a couple of times, by not initialising the required keys with zero values (`CountTable` does that with `inc`), and doing the spawn step at the wrong end of the list.

Had I spotted the table approach during part one, then part two would have been a trivial change.

```nim
proc part1(input: string): int =
  let fish = input.split(",").map(parseInt)
  countFishAfterDays(fish, 80)

proc part2(input: string): int =
  let fish = input.split(",").map(parseInt)
  countFishAfterDays(fish, 256)
```

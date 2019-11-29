package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

// Map from guard to number of minutes spent asleep.
type records = map[int]int

// Map from guard to a map from minute to number of times the guard was
// sleeping at that minute.
type markers = map[int]map[int]int

type log struct {
	guard    int
	minute   int
	sleeping bool
}

func parse(input string) []log {
	lines := strings.Split(input, "\n")
	logs := []log{}
	guard := 0

	// Sort logs into chronological order
	sort.Strings(lines)

	for _, line := range lines {
		r := regexp.MustCompile(`[0-9]+`)
		match := r.FindAllString(line, -1)
		minute, _ := strconv.Atoi(match[4])

		if len(match) > 5 {
			guard, _ = strconv.Atoi(match[5])
			continue
		}

		sleeping := false

		if strings.Contains(line, "asleep") {
			sleeping = true
		}

		logs = append(logs, log{
			guard:    guard,
			minute:   minute,
			sleeping: sleeping,
		})
	}

	return logs
}

func analyse(input string) (records, markers) {
	records, markers := records{}, markers{}
	fellAsleep := 0

	for _, log := range parse(input) {
		if log.sleeping {
			fellAsleep = log.minute
		}

		if !log.sleeping {
			start, end := fellAsleep, log.minute
			records[log.guard] += end - start

			if markers[log.guard] == nil {
				markers[log.guard] = map[int]int{}
			}

			for m := start; m < end; m++ {
				markers[log.guard][m] += 1
			}
		}
	}

	return records, markers
}

func solvePartOne(input string) int {
	records, markers := analyse(input)
	guard := 0

	for id, duration := range records {
		if duration > records[guard] {
			guard = id
		}
	}

	minute := 0

	for m, _ := range markers[guard] {
		if markers[guard][m] > markers[guard][minute] {
			minute = m
		}
	}

	return guard * minute
}

func solvePartTwo(input string) int {
	_, markers := analyse(input)
	guard, minute := 0, 0

	for id, _ := range markers {
		for m, _ := range markers[id] {
			if markers[id][m] > markers[guard][minute] {
				guard, minute = id, m
			}
		}
	}

	return guard * minute
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", solvePartOne(input))
	fmt.Println("Part 2:", solvePartTwo(input))
}

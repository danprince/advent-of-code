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

type Log struct {
	Guard    int
	Minute   int
	Sleeping bool
}

func ParseLogs(input string) []Log {
	lines := strings.Split(input, "\n")
	logs := []Log{}
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

		logs = append(logs, Log{
			Guard:    guard,
			Minute:   minute,
			Sleeping: sleeping,
		})
	}

	return logs
}

// Map from guard to number of minutes spent asleep.
type Records = map[int]int

// Map from guard to a map from minute to number of times the guard was
// sleeping at that minute.
type Markers = map[int]map[int]int

func SleepAnalysis(input string) (Records, Markers) {
	records := Records{}
	markers := Markers{}
	fellAsleep := 0

	for _, log := range ParseLogs(input) {
		if log.Sleeping {
			fellAsleep = log.Minute
		}

		if !log.Sleeping {
			start := fellAsleep
			end := log.Minute
			records[log.Guard] += end - start

			if markers[log.Guard] == nil {
				markers[log.Guard] = map[int]int{}
			}

			for m := start; m < end; m++ {
				markers[log.Guard][m] += 1
			}
		}
	}

	return records, markers
}

func SolvePartOne(input string) int {
	records, markers := SleepAnalysis(input)

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

func SolvePartTwo(input string) int {
	_, markers := SleepAnalysis(input)

	guard := 0
	minute := 0

	for id, _ := range markers {
		for m, _ := range markers[id] {
			if markers[id][m] > markers[guard][minute] {
				guard = id
				minute = m
			}
		}
	}

	return guard * minute
}

func main() {
	bytes, _ := ioutil.ReadAll(os.Stdin)
	input := strings.TrimSpace(string(bytes))
	fmt.Println("Part 1:", SolvePartOne(input))
	fmt.Println("Part 2:", SolvePartTwo(input))
}

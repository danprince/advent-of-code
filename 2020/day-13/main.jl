using Test

function part1(input)
    lines = split(input)
    time = parse(Int, lines[1])
    parts = split(lines[2], ",")
    buses = [parse(Int, bus) for bus in parts if bus != "x"]

    next_bus = first(buses)
    shortest_wait = Inf

    for bus in buses
        wait = bus - time % bus
        if wait < shortest_wait
            next_bus = bus
            shortest_wait = wait
        end
    end

    next_bus * shortest_wait
end

function part2(input)
    lines = split(input)
    parts = split(lines[2], ",")
    buses = [bus == "x" ? bus : parse(Int, bus) for bus in parts]
    step = 1
    time = 1
    n = 1

    while n <= length(buses)
        bus = buses[n]

        # An x in the schedule means there are no constraints on what
        # bus IDs must depart at that time.
        if bus == "x"
            n += 1
            continue
        end

        # use n to find the time at which this bus should be departing
        # (wait will be 0)
        wait = (time + n - 1) % bus

        if wait == 0
            # start the search for the next bus
            n += 1

            # change the step to go directly to the next occurrence of
            # this pattern.
            step *= bus
        else
            # only advance time if we didn't find a match (we might find
            # multiple matching buses at a single time.)
            time += step
        end
    end

    return time
end

open("input.txt") do file
    input = read(file, String)
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end

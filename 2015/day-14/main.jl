mutable struct Reindeer
    name::String
    speed::Int
    flight_time::Int
    rest_time::Int
    flight_timer::Int
    rest_timer::Int
    resting::Bool
    distance::Int
    points::Int
end

function Reindeer(input::AbstractString)
    rx = r"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds."
    m = match(rx, input)
    name = m.captures[1]
    speed, flight_time, rest_time = parse.(Int, m.captures[2:4])
    Reindeer(name, speed, flight_time, rest_time, flight_time, 0, false, 0, 0)
end

function race!(reindeers)
    leader = reindeers[1]

    for i in 1:2503
        for r in reindeers
            if r.resting
                r.rest_timer -= 1

                if r.rest_timer == 0
                    r.resting = false
                    r.flight_timer = r.flight_time
                end
            else
                r.flight_timer -= 1
                r.distance += r.speed

                if r.flight_timer == 0
                    r.resting = true
                    r.rest_timer = r.rest_time
                end
            end

            if r.distance > leader.distance
                leader = r
            end
        end

        for r in reindeers
            if r.distance == leader.distance
                r.points += 1
            end
        end
    end
end

part1(reindeers) = maximum(r -> r.distance, reindeers)
part2(reindeers) = maximum(r -> r.points, reindeers)

open("input.txt") do file
    input = read(file, String) |> strip
    lines = split(input, "\n")
    reindeers = Reindeer.(lines)
    race!(reindeers)

    println("Part 1: $(part1(reindeers))")
    println("Part 2: $(part2(reindeers))")
end

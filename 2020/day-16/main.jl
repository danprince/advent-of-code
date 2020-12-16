const Ticket = Vector{Int}
const Fields = Dict{AbstractString, Any}

function parse_input(input)
    sections = split(input, "\n\n")
    fields = Dict()

    for m in eachmatch(r"(.+): (\d+)-(\d+) or (\d+)-(\d+)", sections[1])
        name = m[1]
        r1 = parse(Int, m[2]):parse(Int, m[3])
        r2 = parse(Int, m[4]):parse(Int, m[5])
        fields[name] = (r1, r2)
    end

    your_ticket::Ticket = begin
        lines = split(sections[2], '\n')
        numbers = split(lines[2], ',')
        parse.(Int, numbers)
    end

    nearby_tickets::Vector{Ticket} = begin
        lines = split(sections[3], '\n')
        lines = lines[2:end]
        map(lines) do line
            numbers = split(line, ',')
            parse.(Int, numbers)
        end
    end

    (fields, your_ticket, nearby_tickets)
end

function part1(fields, tickets)
    sum(tickets) do ticket
        sum(ticket) do value
            for (name, ranges) in fields
                for range in ranges
                    if value in range
                        return 0
                    end
                end
            end

            return value
        end
    end
end

function part2(fields, tickets, your_ticket)
    valid_tickets = filter(tickets) do ticket
        all(ticket) do value
            any(fields) do (name, ranges)
                any(ranges) do range
                    value in range
                end
            end
        end
    end

    candidates = Dict()
    mappings = Dict()

    for (name, ranges) in fields
        indexes = filter(1:length(fields)) do i
            all(valid_tickets) do ticket
                any(ranges) do range
                    ticket[i] in range
                end
            end
        end

        candidates[name] = Set(indexes)
    end

    while length(mappings) < length(fields)
        for (name, indexes) in candidates
            if length(indexes) == 1
                index = first(indexes)
                mappings[name] = index

                for (name, indexes) in candidates
                    delete!(indexes, index)
                end
            end
        end
    end

    target_fields = filter(fields |> keys) do name
        startswith(name, "departure")
    end

    prod(target_fields) do name
        index = mappings[name]
        your_ticket[index]
    end
end

open("input.txt") do file
    input = read(file, String) |> strip

    fields, your_ticket, nearby_tickets = parse_input(input)
    tickets = [your_ticket, nearby_tickets...]

    @time p1 = part1(fields, tickets)
    println("Part 1: $p1")

    @time p2 = part2(fields, tickets, your_ticket)
    println("Part 2: $p2")
end

using Test

const Point3D = Tuple{Int, Int, Int}
const Point4D = Tuple{Int, Int, Int, Int}

mutable struct State3D
    cubes::Set{Point3D}
    swap::Set{Point3D}
    min::Point3D
    max::Point3D
end

mutable struct State4D
    cubes::Set{Point4D}
    swap::Set{Point4D}
    min::Point4D
    max::Point4D
end

function State3D(input::AbstractString)
    swap = Set()
    lines = split(input)

    w = length(lines[1])
    h = length(lines)

    cubes = Set(
        (x-1, y-1, 0)
        for y in 1:h, x in 1:w
        if lines[y][x] == '#'
    )

    State3D(cubes, swap, (0, 0, 0), (w, h, 0))
end

function State4D(input::AbstractString)
    swap = Set()
    lines = split(input)

    w = length(lines[1])
    h = length(lines)

    cubes = Set(
        (x-1, y-1, 0, 0)
        for y in 1:h, x in 1:w
        if lines[y][x] == '#'
    )

    State4D(cubes, swap, (0, 0, 0, 0), (w, h, 0, 0))
end

function adjacent((x, y, z)::Point3D)
    ((x + dx, y + dy, z + dz) for dx in -1:1, dy in -1:1, dz in -1:1 if !(dx == dy == dz == 0))
end

function adjacent((x, y, z, w)::Point4D)
    ((x + dx, y + dy, z + dz, w + dw) for dx in -1:1, dy in -1:1, dz in -1:1, dw in -1:1 if !(dx == dy == dz == dw == 0))
end

function simulate!(state::State3D)
    x0, y0, z0 = state.min
    x1, y1, z1 = state.max
    new_cubes = state.swap

    for x in x0 - 1 : x1 + 1
        for y in y0 - 1 : y1 + 1
            for z in z0 - 1 : z1 + 1
                point = (x, y, z)
                active = point in state.cubes

                neighbours = count(adjacent(point)) do neighbour
                    neighbour in state.cubes
                end

                activate = if active
                    2 <= neighbours <= 3
                else
                    neighbours == 3
                end

                if activate
                    push!(new_cubes, point)
                else
                    delete!(new_cubes, point)
                end

                state.min = min.(state.min, point)
                state.max = max.(state.max, point)
            end
        end
    end

    state.swap = state.cubes
    state.cubes = new_cubes
end

function simulate!(state::State4D)
    x0, y0, z0, w0 = state.min
    x1, y1, z1, w1 = state.max
    new_cubes = state.swap

    for x in x0 - 1 : x1 + 1
        for y in y0 - 1 : y1 + 1
            for z in z0 - 1 : z1 + 1
                for w in w0 - 1 : w1 + 1
                    point = (x, y, z, w)
                    active = point in state.cubes

                    neighbours = count(adjacent(point)) do neighbour
                        neighbour in state.cubes
                    end

                    activate = if active
                        2 <= neighbours <= 3
                    else
                        neighbours == 3
                    end

                    if activate
                        push!(new_cubes, point)
                    else
                        delete!(new_cubes, point)
                    end

                    state.min = min.(state.min, point)
                    state.max = max.(state.max, point)
                end
            end
        end
    end

    state.swap = state.cubes
    state.cubes = new_cubes
end

function part1(input)
    state = State3D(input)

    for cycles in 1:6
        simulate!(state)
    end

    length(state.cubes)
end

function part2(input)
    state = State4D(input)

    for cycles in 1:6
        simulate!(state)
    end

    length(state.cubes)
end

open("example.txt") do file
    input = read(file, String)
    @test part1(input) == 112
    @test part2(input) == 848
end

open("input.txt") do file
    input = read(file, String)
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end

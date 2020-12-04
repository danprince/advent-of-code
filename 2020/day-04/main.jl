using Test

const validators = Dict(
    "byr" => s -> 1920 <= parse(Int, s) <= 2002,
    "iyr" => s -> 2010 <= parse(Int, s) <= 2020,
    "eyr" => s -> 2020 <= parse(Int, s) <= 2030,
    "hgt" => s -> valid_height(s),
    "hcl" => s -> occursin(r"^#[0-9a-f]{6}$", s),
    "ecl" => s -> occursin(r"^(amb|blu|brn|gry|grn|hzl|oth)$", s),
    "pid" => s -> occursin(r"^[0-9]{9}$", s),
)

function part1(input)
    lines = split(input, "\n\n")
    passports = parse_passport.(lines)

    count(passports) do passport
        all(validators) do (field, rule)
            haskey(passport, field)
        end
    end
end

function part2(input)
    lines = split(input, "\n\n")
    passports = parse_passport.(lines)

    count(passports) do passport
        all(validators) do (field, rule)
            try
                value = passport[field]
                rule(value)
            catch
                false
            end
        end
    end
end

function parse_passport(input)
    passport = Dict()

    for part in split(input)
        key, value = split(part, ":")
        passport[key] = value
    end

    passport
end

function valid_height(s)
    suffix = s[end-1:end]
    value = parse(Int, s[1:end-2])

    if suffix == "cm"
        150 <= value <= 193
    else
        59 <= value <= 76
    end
end

@test validators["byr"]("2002")
@test !validators["byr"]("2003")

@test validators["hgt"]("60in")
@test validators["hgt"]("190cm")
@test !validators["hgt"]("190in")
@test !validators["hgt"]("190")

@test validators["ecl"]("brn")
@test !validators["ecl"]("wat")

@test validators["pid"]("000000001")
@test !validators["pid"]("0123456789")

@test part1("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in") == 2


@test part2("eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007") == 0

@test part2("pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719") == 4

open("input.txt") do file
    input = read(file, String)
    println("Part 1: $(part1(input))")
    println("Part 2: $(part2(input))")
end

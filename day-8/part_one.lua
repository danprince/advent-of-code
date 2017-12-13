-- http://adventofcode.com/2017/day/8

local conditions = {
  [">"] = function(a, b) return a > b end,
  ["<"] = function(a, b) return a < b end,
  ["<="] = function(a, b) return a <= b end,
  [">="] = function(a, b) return a >= b end,
  ["=="] = function(a, b) return a == b end,
  ["!="] = function(a, b) return a ~= b end,
}

local commands = {
  ["inc"] = function(a, b) return a + b end,
  ["dec"] = function(a, b) return a - b end
}

function parse_instruction(str)
  local pattern = "(%a+) (%a+) (-?%d+) if (%a+) (.+) (-?%d+)"
  local register, command, value, c_register, c_operator, c_value = string.match(str, pattern)

  local condition = {
    register=c_register,
    operator=c_operator,
    value=tonumber(c_value)
  }

  return {
    register=register,
    command=command,
    value=tonumber(value),
    condition=condition
  }
end

function parse_program(str)
  local instructions = {}

  for line in str:gmatch("[^\n]+") do
    local instruction = parse_instruction(line)
    table.insert(instructions, instruction)
  end

  return instructions
end

function initialize_registers(registers)
  local registers = registers or {}
  local metatable = { __index = function() return 0 end }
  return setmetatable(registers, metatable)
end

function evaluate_condition(condition, registers)
  local register = registers[condition.register]  
  local value = condition.value
  return conditions[condition.operator](register, value)
end

function evaluate_instruction(instruction, registers)
  if evaluate_condition(instruction.condition, registers) then
    local command = commands[instruction.command]
    local name = instruction.register
    registers[name] = command(registers[name], instruction.value)
  end
end

function evaluate_program(instructions, registers)
  for _, instruction in pairs(instructions) do
    evaluate_instruction(instruction, registers)
  end
end

function find_max_register(registers)
  local max = ""

  for name, value in pairs(registers) do
    if value > registers[max] then
      max = name
    end
  end

  return registers[max]
end

function solve(input)
  local program = parse_program(input)
  local registers = initialize_registers({})
  evaluate_program(program, registers)
  return find_max_register(registers)
end


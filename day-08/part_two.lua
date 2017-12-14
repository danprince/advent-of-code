-- http://adventofcode.com/2017/day/8

local part_one = require("part_one")

function initialize_memory_registers()
  local max = 0
  local registers = {}

  local metatable = {
    __call = function() return max end,
    __index = function(table, key) return registers[key] or 0 end,
    __newindex = function(table, key, value)
      if value > max then max = value end
      registers[key] = value
    end
  }

  return setmetatable({}, metatable)
end

function solve(input)
  local program = part_one.parse_program(input)
  local registers = initialize_memory_registers()
  part_one.evaluate_program(program, registers)
  return registers()
end

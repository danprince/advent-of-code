// http://adventofcode.com/2017/day/18

interface VirtualMachine {
  registers: { [id: string]: number };
  program: string[][];
  inbox: number[];
  outbox: number[];
  index: number;
}

namespace Parse {
  export function instruction(src) {
    return src.split(" ");
  }

  export function program(src) {
    return src.split("\n").map(Parse.instruction);
  }
}

namespace Registers {
  export function get(registers, id) {
    return id in registers ? registers[id] : parseInt(id);
  }
}

namespace VirtualMachine {
  export function create(src, inbox = [], outbox = []): VirtualMachine {
    return {
      registers: {},
      program: Parse.program(src),
      index: 0,
      inbox,
      outbox
    };
  }

  export function execute(vm: VirtualMachine) {
    let [name, x, y] = vm.program[vm.index];
    let xv = Registers.get(vm.registers, x);
    let yv = Registers.get(vm.registers, y);

    switch (name) {
      case "set":
        vm.registers[x] = yv;
        break;
      case "add":
        vm.registers[x] += yv;
        break;
      case "mul":
        vm.registers[x] *= yv;
        break;
      case "mod":
        vm.registers[x] %= yv;
        break;
      case "snd":
        vm.outbox.push(xv);
        break;
      case "rcv":
        if (vm.inbox.length > 0) vm.registers[x] = vm.inbox.shift();
        else return;
        break;
      case "jgz":
        if (xv > 0) vm.index += yv - 1;
        break;
    }

    vm.index += 1;
  }

  export function isWaiting(vm: VirtualMachine) {
    return vm.inbox.length === 0 && vm.program[vm.index][0] === "rcv";
  }

  export function run(vm: VirtualMachine) {
    while (!VirtualMachine.isWaiting(vm)) {
      VirtualMachine.execute(vm);
    }
  }
}

function solve(input) {
  let vm = VirtualMachine.create(input);
  VirtualMachine.run(vm);
  return vm.outbox.pop();
}

export { VirtualMachine };

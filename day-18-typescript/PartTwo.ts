// http://adventofcode.com/2017/day/18

import { VirtualMachine } from "./PartOne";

export function isDeadlocked(vm1: VirtualMachine, vm2: VirtualMachine) {
  return VirtualMachine.isWaiting(vm1) && VirtualMachine.isWaiting(vm2);
}

function solve(input) {
  let inbox = [];
  let outbox = [];
  let vm1 = VirtualMachine.create(input, inbox, outbox);
  let vm2 = VirtualMachine.create(input, outbox, inbox);
  let score = 0;
  vm1.registers.p = 0;
  vm2.registers.p = 1;

  while (!isDeadlocked(vm1, vm2)) {
    VirtualMachine.run(vm1);
    VirtualMachine.run(vm2);
    score += vm2.outbox.length;
  }

  return score;
}

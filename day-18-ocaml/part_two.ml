(* http://adventofcode.com/2017/day/18 *)

#use "part_one.ml"

open Printf

module VM = struct
  type t =
    { registers: Registers.t;
      program: (string * string * string) array;
      inbox: int Event.channel;
      outbox: int Event.channel;
      return: int;
      index: int;
      value: int;
      sent: int;
    }

  let make src inbox outbox =
    { registers = Hashtbl.create 16;
      program = Parse.program src;
      inbox = inbox;
      outbox = outbox;
      return = 0;
      index = 0;
      value = 0;
      sent = 0;
    }

  let execute vm =
    let (name, x, y) = Array.get vm.program vm.index in
    let x_value = Registers.get vm.registers x in
    let y_value = Registers.get vm.registers y in
    let set = Registers.set vm.registers x in
    let id = Thread.id (Thread.self ()) in
    match name with
    | "set" -> { vm with registers = set y_value }
    | "add" -> { vm with registers = set (x_value + y_value) }
    | "mul" -> { vm with registers = set (x_value * y_value) }
    | "mod" -> { vm with registers = set (x_value mod y_value) }
    | "rcv" -> { vm with registers = set (Event.sync (Event.receive vm.inbox)) }
    | "snd" -> ( Event.sync (Event.send vm.outbox x_value) ; vm )
    | "jgz" -> if x_value > 0 then { vm with index = vm.index + y_value - 1 } else vm
    | _ -> vm

  let step vm =
    let next = execute vm in
    { next with index = next.index + 1 }

  let rec run vm =
    run (step vm)
end

let solve input =
  let inbox = Event.new_channel () in
  let outbox = Event.new_channel () in
  let v1 = VM.make input inbox outbox in
  let v2 = VM.make input outbox inbox in begin
    Registers.set v1.registers "p" 0 ;
    Registers.set v2.registers "p" 1 ;
    Thread.create VM.run v1 ;
    Thread.create VM.run v2 ;
  end


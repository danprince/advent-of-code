(* http://adventofcode.com/2017/day/18 *)

module Parse = struct
  let instruction str =
    let parts = Str.split (Str.regexp " ") str in

    match parts with
    | [name; x; y] -> (name, x, y)
    | [name; x] -> (name, x, "")
    | _ -> ("", "", "")

  let program src =
    let lines = Str.split (Str.regexp "\n") src in
    let instructions = List.map instruction lines in
    Array.of_list instructions
end

module Registers = struct
  type t = (string, int) Hashtbl.t

  let parse_int str =
    match int_of_string_opt str with
    | Some n -> n
    | None -> 0

  let get registers id =
    match Hashtbl.find_opt registers id with
    | Some num -> num
    | None -> parse_int id

  let set registers id value = begin
    Hashtbl.add registers id value ;
    registers
  end
end

module VM = struct
  type t =
    { registers: Registers.t;
      program: (string * string * string) array;
      return: int;
      index: int;
      value: int;
      sent: int;
    }

  let make src =
    { registers = Hashtbl.create 16;
      program = Parse.program src;
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

    match name with
    | "set" -> { vm with registers = set y_value }
    | "add" -> { vm with registers = set (x_value + y_value) }
    | "mul" -> { vm with registers = set (x_value * y_value) }
    | "mod" -> { vm with registers = set (x_value mod y_value) }
    | "snd" -> { vm with sent = x_value }
    | "rcv" -> if x_value > 0 then { vm with return = vm.sent } else vm
    | "jgz" -> if x_value > 0 then { vm with index = vm.index + y_value - 1 } else vm
    | _ -> vm

  let step vm =
    let next = execute vm in
    { next with index = next.index + 1 }

  let rec run vm =
    match vm.return with
    | 0 -> run (step vm)
    | _ -> vm
end

let solve input =
  let vm = VM.make input in
  let result = VM.run vm in
  result.return

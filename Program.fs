// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Elmish

module Mk61Model =

  type RegisterValue =
  | Entry of string
  | Number of float
  | Error

  module RegisterValue =
    let toNumber =
      function
      | Number x -> Number x
      | Entry s ->
        match Double.TryParse s with
        | true, n -> Number n
        | _ -> Error
      | _ -> Error

    let rec add x1 x2 =
      match x1, x2 with
      | Number x1', Number x2' -> x1' + x2' |> Number
      | Entry _, _ -> add (x1 |> toNumber) x2
      | _, Entry _ -> add x1 (x2 |> toNumber)
      | _ -> Error

    let rec subtract x1 x2 =
      match x1, x2 with
      | Number x1', Number x2' -> x1' - x2' |> Number
      | Entry _, _ -> subtract (x1 |> toNumber) x2
      | _, Entry _ -> subtract x1 (x2 |> toNumber)
      | _ -> Error

    let rec multiply x1 x2 =
      match x1, x2 with
      | Number x1', Number x2' -> x1' * x2' |> Number
      | Entry _, _ -> multiply (x1 |> toNumber) x2
      | _, Entry _ -> multiply x1 (x2 |> toNumber)
      | _ -> Error

    let rec divide x1 x2 =
      match x1, x2 with
      | Number _, Number 0.0 -> Error
      | Number x1', Number x2' -> x1' / x2' |> Number
      | Entry _, _ -> divide (x1 |> toNumber) x2
      | _, Entry _ -> divide x1 (x2 |> toNumber)
      | _ -> Error


  type Msg =
  | Clear
  | KeyPress of ConsoleKey
  | NumberEntry of string
  | Push
  | Plus
  | Multiply
  | Divide
  | Minus
  | WaitUserActivity
  | Exit

  type State = {
    Registers : RegisterValue list
    Log : Result<unit, string>
    LastMsg : Msg }

  type UpdateResult = State * Cmd<Msg>

  module UpdateResult =
    let setLastMsg msg ((state: State), (cmd : Cmd<Msg>)) =
      { state with LastMsg = msg }, cmd

module Mk61Init =
  open Mk61Model
  let init () =
    {Registers = [Number 0.0] ; Log = Ok () ; LastMsg = Clear }, WaitUserActivity |> Cmd.ofMsg


module Mk61Update =
  open Mk61Model
  open Mk61Init

  let updateConsoleKey (msg: Msg) (state: State) =
    (match msg with
    | KeyPress ConsoleKey.C -> state, Clear |> Cmd.ofMsg
    | KeyPress ConsoleKey.D0 -> state, NumberEntry "0" |> Cmd.ofMsg
    | KeyPress ConsoleKey.D1 -> state, NumberEntry "1" |> Cmd.ofMsg
    | KeyPress ConsoleKey.D2 -> state, NumberEntry "2" |> Cmd.ofMsg
    | KeyPress ConsoleKey.D3 -> state, NumberEntry "3" |> Cmd.ofMsg
    | KeyPress ConsoleKey.D4 -> state, NumberEntry "4" |> Cmd.ofMsg
    | KeyPress ConsoleKey.D5 -> state, NumberEntry "5" |> Cmd.ofMsg
    | KeyPress ConsoleKey.D6 -> state, NumberEntry "6" |> Cmd.ofMsg
    | KeyPress ConsoleKey.D7 -> state, NumberEntry "7" |> Cmd.ofMsg
    | KeyPress ConsoleKey.D8 -> state, NumberEntry "8" |> Cmd.ofMsg
    | KeyPress ConsoleKey.D9 -> state, NumberEntry "9" |> Cmd.ofMsg
    | KeyPress ConsoleKey.OemPeriod -> state, NumberEntry "." |> Cmd.ofMsg
    | KeyPress ConsoleKey.Add
    | KeyPress ConsoleKey.OemPlus -> state, Plus |> Cmd.ofMsg
    | KeyPress ConsoleKey.Subtract
    | KeyPress ConsoleKey.OemMinus -> state, Minus |> Cmd.ofMsg
    | KeyPress ConsoleKey.Multiply -> state, Multiply |> Cmd.ofMsg
    | KeyPress ConsoleKey.Divide -> state, Divide |> Cmd.ofMsg
    | KeyPress ConsoleKey.Enter -> state, Push |> Cmd.ofMsg
    | KeyPress ConsoleKey.Escape -> state, Exit |> Cmd.ofMsg
    | KeyPress _ -> {state with Log = Result.Error "Unknown key pressed" }, WaitUserActivity |> Cmd.ofMsg
    | msg' ->
      {state with Log = msg' |> sprintf "Unexpected message '%A' in updateConsoleKey" |> Result.Error },
      WaitUserActivity |> Cmd.ofMsg)
    // |> UpdateResult.setLastMsg msg




  let updateNumberEntry (msg: Msg) (state: State) =
    (match msg, state with
    | NumberEntry d, { Registers = Entry e :: stateTail } ->
      { state with Registers = (e + d |> Entry) :: stateTail ; Log = Ok () },
      WaitUserActivity |> Cmd.ofMsg
    // skip auto push if last msg was a push, as opposed to the case after this
    | NumberEntry d, { Registers = Number _ :: stateTail ; LastMsg = Push}
    | NumberEntry d, { Registers = Number _ :: stateTail ; LastMsg = Clear} ->
      { state with Registers = (d |> Entry) :: stateTail ; Log = Ok () },
      WaitUserActivity |> Cmd.ofMsg
    | NumberEntry d, { Registers = Number n :: stateTail} ->
      { state with Registers = (d |> Entry) :: (Number n) :: stateTail ; Log = Ok () },
      WaitUserActivity |> Cmd.ofMsg
    | NumberEntry d, { Registers = Error :: stateTail} ->
      { state with Registers = (d |> Entry) :: stateTail ; Log = Ok () },
      WaitUserActivity |> Cmd.ofMsg
    | msg', _ ->
      { state with Log = msg' |> sprintf "Unexpected message '%A' in updateNumberEntry" |> Result.Error },
      WaitUserActivity |> Cmd.ofMsg)
    |> UpdateResult.setLastMsg msg

  let updatePush (msg: Msg) (state: State) =
    (match msg, state with
    | Push, {Registers = Number n :: stateTail} ->
      { state with Registers = (Number n) :: (Number n) :: stateTail ; Log = Ok () },
      WaitUserActivity |> Cmd.ofMsg
    | Push, {Registers = Entry e :: stateTail } ->
      match Double.TryParse e with
      | true, n ->
        { state with Registers = (Number n) :: (Number n) :: stateTail ; Log = Ok () },
        WaitUserActivity |> Cmd.ofMsg
      | _ ->
        { state with Registers = (Number 0.0 ) :: stateTail ; Log = e |> sprintf "'%s' is not a valid number, resetting it to zero" |> Result.Error},
        WaitUserActivity |> Cmd.ofMsg
    | Push, {Registers = Error :: _ ; Log = _ } ->
      { state with Log = "Cannot push Error value" |> Result.Error },
      WaitUserActivity |> Cmd.ofMsg
    | msg', _ ->
      { state with Log = msg' |> sprintf "Unexpected message '%A' in updatePush" |> Result.Error},
      WaitUserActivity |> Cmd.ofMsg)
    |> UpdateResult.setLastMsg msg

  let updatePlus (msg: Msg) (state: State) =
    (match msg, state with
    | Plus, {Registers = x :: y :: stateTail } ->
      { state with Registers = (RegisterValue.add x y) :: stateTail ; Log = Ok () },
      WaitUserActivity |> Cmd.ofMsg
    | Plus, {Registers = x :: stateTail} ->
        { state with Registers = (RegisterValue.add (Number 0.0) x) :: stateTail ; Log = Ok () },
        WaitUserActivity |> Cmd.ofMsg
    | msg', _ ->
      { state with Log = msg' |> sprintf "Unexpected message '%A' in updatePlus" |> Result.Error},
      WaitUserActivity |> Cmd.ofMsg)
    |> UpdateResult.setLastMsg msg

  let updateMinus (msg: Msg) (state: State) =
    (match msg, state with
    | Minus, {Registers = x :: y :: stateTail } ->
      { state with Registers = (RegisterValue.subtract y x) :: stateTail ; Log = Ok () },
      WaitUserActivity |> Cmd.ofMsg
    | Minus, {Registers = x :: stateTail} ->
        { state with Registers = (RegisterValue.subtract (Number 0.0) x) :: stateTail ; Log = Ok () },
        WaitUserActivity |> Cmd.ofMsg
    | msg', _ ->
      { state with Log = msg' |> sprintf "Unexpected message '%A' in updateMinus" |> Result.Error},
      WaitUserActivity |> Cmd.ofMsg)
    |> UpdateResult.setLastMsg msg

  let updateMultiply (msg: Msg) (state: State) =
    (match msg, state with
    | Multiply, {Registers = x :: y :: stateTail } ->
      { state with Registers = (RegisterValue.multiply x y) :: stateTail ; Log = Ok () },
      WaitUserActivity |> Cmd.ofMsg
    | Multiply, {Registers = x :: stateTail} ->
        { state with Registers = (RegisterValue.multiply (Number 1.0) x) :: stateTail ; Log = Ok () },
        WaitUserActivity |> Cmd.ofMsg
    | msg', _ ->
      { state with Log = msg' |> sprintf "Unexpected message '%A' in updateMultiply" |> Result.Error},
      WaitUserActivity |> Cmd.ofMsg)
    |> UpdateResult.setLastMsg msg

  let updateDivide (msg: Msg) (state: State) =
    (match msg, state with
    | Divide, {Registers = x :: y :: stateTail } ->
      { state with Registers = (RegisterValue.divide y x) :: stateTail ; Log = Ok () },
      WaitUserActivity |> Cmd.ofMsg
    | Divide, {Registers = x :: stateTail} ->
        { state with Registers = (RegisterValue.divide (Number 1.0) x) :: stateTail ; Log = Ok () },
        WaitUserActivity |> Cmd.ofMsg
    | msg', _ ->
      { state with Log = msg' |> sprintf "Unexpected message '%A' in updateMultiply" |> Result.Error},
      WaitUserActivity |> Cmd.ofMsg)
    |> UpdateResult.setLastMsg msg

  let updateWaitUserActivity (state: State) =
    (state, Console.ReadKey().Key |> KeyPress |> Cmd.ofMsg)
    // |> UpdateResult.setLastMsg WaitUserActivity

  let update (msg: Msg) (state: State) =
    match msg, state with
    | WaitUserActivity, _ -> updateWaitUserActivity state
    | KeyPress _, _ -> updateConsoleKey msg state
    | Clear, _ -> init ()
    | NumberEntry _, _ ->
      updateNumberEntry msg state
    | Push, _ ->
      updatePush msg state
    | Plus, _ ->
      updatePlus msg state
    | Minus, _ ->
      updateMinus msg state
    | Multiply, _ ->
      updateMultiply msg state
    | Divide, _ ->
      updateDivide msg state
    | Exit, _ ->
      { state with Log = "Exit" |> Result.Error }, []
    | msg', _ ->
      { state with Log = msg' |> sprintf "Fatal error or not implemented message '%A'." |> Result.Error }, WaitUserActivity |> Cmd.ofMsg

module Mk61View =
  open Mk61Model
  let view (state : State) _ =
    Console.Clear()
    Console.ForegroundColor <- ConsoleColor.Blue
    let h :: t = state.Registers
    t
    |> List.rev
    |> List.iter (fun v -> Console.WriteLine("REG: {0}", v))
    Console.ForegroundColor <- ConsoleColor.Green
    Console.WriteLine("OUT: {0}", h)
    Console.ForegroundColor <- ConsoleColor.Gray
    Console.Write("LOG: ")
    match state.Log with
    | Ok () ->
      Console.ForegroundColor <- ConsoleColor.Green
      Console.WriteLine("OK")
    | Result.Error err ->
      Console.ForegroundColor <- ConsoleColor.Red
      Console.WriteLine("{0}", err)
    Console.ForegroundColor <- ConsoleColor.Yellow
    Console.WriteLine("LST: {0}", state.LastMsg)

open Mk61Update
open Mk61View
open Mk61Init

[<EntryPoint>]
let main argv =
  let p =
    Program.mkProgram init update view
    |> Program.run

  0 // return an integer exit code
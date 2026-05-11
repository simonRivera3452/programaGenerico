module Generic.Saludo

open System

open Generic.Utils

type ProgramState = 
| Running
| Terminated


type EntryState =
| AskingForData
| ShowingData



type State = {
    ProgramState: ProgramState
    Tick: int
    Clock: int
    RedrawScreen: bool
    EntryState: EntryState
    EntryX: int
    EntryY: int
    EntryData: string
    EntryLabel: string
}

let initialState = {
    ProgramState = Running
    Tick = -1
    Clock = 0
    RedrawScreen = true
    EntryState = AskingForData
    EntryX = 0
    EntryY = 15
    EntryData = ""
    EntryLabel = "Entra tu nombre: "
}

let updateTick state =
    {state with Tick = state.Tick+1}

let updateClock state =
    if state.Tick <> 0 && state.Tick % 40 = 0 then 
        {state with Clock=state.Clock+1;RedrawScreen=true}
    else
        state

let updateSaludoKeyboard key state =
    match key with 
    | ConsoleKey.Escape -> {state with ProgramState=Terminated}
    | _ -> state

let updateEntryKeyboard (key: ConsoleKeyInfo) state =
    if state.EntryState = AskingForData then 
        match key with
        | k when Char.IsLetter k.KeyChar ->
            {state with EntryData = state.EntryData+key.KeyChar.ToString(); RedrawScreen=true}
        | k ->
            match k.Key with 

            | ConsoleKey.Spacebar ->
                {state with EntryData = state.EntryData+key.KeyChar.ToString(); RedrawScreen=true}
            | ConsoleKey.Backspace ->
                    let indexParaBorrar = max 0 (state.EntryData.Length - 1)
                    if state.EntryData.Length > 0 then
                        { state with 
                            EntryData = state.EntryData.Remove(indexParaBorrar, 1)
                            RedrawScreen = true }
                    else
                        state // Si ya está vacía, devolvemos el estado sin cambios y no explota
            | ConsoleKey.Enter ->
                { state with EntryState = ShowingData;RedrawScreen=true}
            | _ -> state
    else
        state



let redrawClock state =
    displayMessageRight 0 ConsoleColor.Yellow $"{state.Clock}"


let redrawEntry state =
    match state.EntryState with 
    | AskingForData ->
        displayMessage state.EntryX state.EntryY ConsoleColor.Red state.EntryLabel
        displayMessage (state.EntryX+state.EntryLabel.Length) state.EntryY ConsoleColor.Blue state.EntryData
        displayMessage (state.EntryX+state.EntryLabel.Length+state.EntryData.Length) state.EntryY ConsoleColor.Red "☠️"
    | ShowingData ->
        displayMessage state.EntryX state.EntryY ConsoleColor.Cyan $"Hola {state.EntryData}"

let pipeline = [|
    updateTick
    updateClock
|]
let myLoop = 
    createMainLoop 
        pipeline 
        (fun s -> s.ProgramState <> Terminated)
        [|
            (fun (k: ConsoleKeyInfo) s -> updateSaludoKeyboard k.Key s)
            (fun (k: ConsoleKeyInfo) s -> updateEntryKeyboard k s)|]
        [| redrawClock;redrawEntry|]
        (fun s -> s.RedrawScreen)
        (fun s -> {s with RedrawScreen=false})


let mostrar() =
    Console.Clear()
    Console.CursorVisible <- false

    initialState 
    |> myLoop
    |> ignore

    Console.CursorVisible <- true
    Console.Clear()
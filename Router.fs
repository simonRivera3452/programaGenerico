module Generic.Router
open Generic.Utils
open Generic.Types
//
// La funcion de este modulo es decidir
// que se muestra en la pantalla
//

type RouterState =
| ShowingMenu
| ShowingRock
| ShowingMonster
| ShowingSaludo
| Terminated

let initialState = ShowingMenu

let rec mainLoop state =
    let nextState = // 1. Guardamos el resultado del match en una variable
        match state with 
        | ShowingMenu -> 
            match Generic.Menu.mostrar 
                10 
                5 
                [| 
                (NewRockSim, "Show Rock")
                (NewMonsterSim, "Show Monster") 
                (NewSaludo, "Show Saludo") 
                (Exit, "Salir") 
                |] with 
            | NewRockSim -> ShowingRock
            | NewMonsterSim -> ShowingMonster
            | NewSaludo -> ShowingSaludo
            | Exit -> Terminated
        | ShowingRock -> 
            Generic.Rock.mostrar()
            ShowingMenu
        | ShowingMonster ->
            Monster.mostrar()
            ShowingMenu
        | ShowingSaludo ->
            Generic.Saludo.mostrar()
            ShowingMenu
        | Terminated ->
            Terminated

    if nextState <> Terminated then
        mainLoop nextState


let mostrar() =
    initialState
    |> mainLoop
open System

type Direction = Up | Right | Down | Left
type TurnDirection = TurnLeft | GoStraight | TurnRight

type StraightTrackType = Vertical | Horizontal
type CurvedTrackType = FSlash | BSlash
type Track = StraightTrack of StraightTrackType | CurvedTrack of CurvedTrackType | Intersection

type TrackBeneath = Track
type Place = Track of Track | Cart of (Direction * TurnDirection * TrackBeneath)

let readLines filePath = IO.File.ReadLines(filePath)

let charToPlace c =
    match c with
    | '-' -> Track (StraightTrack Horizontal) |> Some
    | '|' -> Track (StraightTrack Vertical) |> Some
    | '\\' -> Track (CurvedTrack BSlash) |> Some
    | '/' -> Track (CurvedTrack FSlash) |> Some
    | '+' -> Track Intersection |> Some
    | '^' -> Cart (Up, TurnLeft, StraightTrack Vertical) |> Some
    | '>' -> Cart (Right, TurnLeft, StraightTrack Horizontal) |> Some
    | 'v' -> Cart (Down, TurnLeft, StraightTrack Vertical) |> Some
    | '<' -> Cart (Left, TurnLeft, StraightTrack Horizontal) |> Some
    | _   -> None

let parseInput s = Array.map charToPlace (Array.ofSeq s)

let turnRight dir =
    match dir with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let turnLeft dir =
    match dir with
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

let turnCart place =
    match place with
    | Cart (d, GoStraight, t) -> Cart (d, TurnRight, t)
    | Cart (d, TurnLeft, t) -> Cart (turnLeft d, GoStraight, t)
    | Cart (d, TurnRight, t) -> Cart (turnRight d, TurnLeft, t)
    | _ -> place

let directionEq place dir =
    match (place, dir) with
    | (Some (Cart (Down, _, _)), Down)
    | (Some (Cart (Left, _, _)), Left)
    | (Some (Cart (Right, _, _)), Right)
    | (Some (Cart (Up, _, _)), Up) -> true
    | _ -> false

//let printTracks tracks =
//    Console.CursorVisible <- false
//    Array.iteri (fun y row ->
//        if y < 50 then
//            Array.iteri (fun x place ->
//                match place with
//                | Some (Track (StraightTrack Horizontal)) -> writeCharacter '-' x y ConsoleColor.Gray
//                | Some (Track (StraightTrack Vertical)) -> writeCharacter '|' x y ConsoleColor.Gray
//                | Some (Track (CurvedTrack FSlash)) -> writeCharacter '/' x y ConsoleColor.Gray
//                | Some (Track (CurvedTrack BSlash)) -> writeCharacter '\\' x y ConsoleColor.Gray
//                | Some (Track Intersection) -> writeCharacter '+' x y ConsoleColor.Gray
//                | Some (Cart (Left, _, _)) -> writeCharacter '<' x y ConsoleColor.Blue
//                | Some (Cart (Up, _, _)) -> writeCharacter '^' x y ConsoleColor.Blue
//                | Some (Cart (Right, _, _)) -> writeCharacter '>' x y ConsoleColor.Blue
//                | Some (Cart (Down, _, _)) -> writeCharacter 'v' x y ConsoleColor.Blue
//                | _ -> writeCharacter ' ' x y ConsoleColor.Gray)
//                row
//            printfn "")
//        tracks
//    printfn ""

let printTracks tracks =
    Console.CursorVisible <- false
    Array.iteri (fun y row ->
            Array.iteri (fun x place ->
                match place with
                | Some (Track (StraightTrack Horizontal)) -> printf "%c" '-'
                | Some (Track (StraightTrack Vertical)) -> printf "%c" '|'
                | Some (Track (CurvedTrack FSlash)) -> printf "%c" '/'
                | Some (Track (CurvedTrack BSlash)) -> printf "%c" '\\'
                | Some (Track Intersection) -> printf "%c" '+'
                | Some (Cart (Left, _, _)) -> printf "%c" '<'
                | Some (Cart (Up, _, _)) -> printf "%c" '^'
                | Some (Cart (Right, _, _)) -> printf "%c" '>'
                | Some (Cart (Down, _, _)) -> printf "%c" 'v'
                | _ -> printf "%c" ' ')
                row
            printfn "")
        tracks
    printfn ""

let updatePlace (tracks:Place option [][]) y x (place:Place option) : unit =
    //let up = if y > 0 && x < tracks.[y - 1].Length then tracks.[y - 1].[x] else None
    //let down = if y < tracks.Length - 1 && x < tracks.[y + 1].Length then tracks.[y + 1].[x] else None
    //let left = if x > 0 then tracks.[y].[x - 1] else None
    //let right = if x < tracks.[y].Length - 1 then tracks.[y].[x + 1] else None
    //printTracks tracks
    match place with
    | None -> ()
    | Some (Track _) -> ()
    | Some (Cart (Left, td, t)) ->
        match tracks.[y].[x-1] with
        | Some (Cart (_, _, t)) -> tracks.[y].[x-1] <- Some (Track t)
        | Some (Track track) ->
            tracks.[y].[x-1] <-
                match track with
                | CurvedTrack FSlash -> Some (Cart (Down, td, CurvedTrack FSlash))
                | CurvedTrack BSlash -> Some (Cart (Up, td, CurvedTrack BSlash))
                | Intersection -> Some (turnCart (Cart (Left, td, Intersection)))
                | _ -> Some (Cart (Left, td, track))
        tracks.[y].[x] <- Some (Track t)
    | Some (Cart (Right, td, t)) ->
        match tracks.[y].[x+1] with
        | Some (Cart (_, _, t)) -> tracks.[y].[x+1] <- Some (Track t)
        | Some (Track track) ->
            tracks.[y].[x+1] <-
                match track with
                | CurvedTrack FSlash -> Some (Cart (Up, td, CurvedTrack FSlash))
                | CurvedTrack BSlash -> Some (Cart (Down, td, CurvedTrack BSlash))
                | Intersection -> Some (turnCart (Cart (Right, td, Intersection)))
                | _ -> Some (Cart (Right, td, track))
        tracks.[y].[x] <- Some (Track t)
    | Some (Cart (Up, td, t)) ->
        match tracks.[y-1].[x] with
        | Some (Cart (_, _, t)) -> tracks.[y-1].[x] <- Some (Track t)
        | Some (Track track) ->
            tracks.[y-1].[x] <-
                match track with
                | CurvedTrack FSlash -> Some (Cart (Right, td, CurvedTrack FSlash))
                | CurvedTrack BSlash -> Some (Cart (Left, td, CurvedTrack BSlash))
                | Intersection -> Some (turnCart (Cart (Up, td, Intersection)))
                | _ -> Some (Cart (Up, td, track))
        tracks.[y].[x] <- Some (Track t)
    | Some (Cart (Down, td, t)) ->
        match tracks.[y+1].[x] with
        | Some (Cart (_, _, t)) -> tracks.[y+1].[x] <- Some (Track t)
        | Some (Track track) ->
            tracks.[y+1].[x] <-
                match track with
                | CurvedTrack FSlash -> Some (Cart (Left, td, CurvedTrack FSlash))
                | CurvedTrack BSlash -> Some (Cart (Right, td, CurvedTrack BSlash))
                | Intersection -> Some (turnCart (Cart (Down, td, Intersection)))
                | _ -> Some (Cart (Down, td, track))
        tracks.[y].[x] <- Some (Track t)
    //| None -> place
    //| Some (Cart (Right, _, t)) ->
    //    match (up, down, left, right) with
    //    | (_, _, _, Some (Cart _)) -> Track t |> Some
    //    | _ ->
    //| Some (Cart (_, _, t)) ->
    //    match (up, down, left, right) with
    //    | (Some (Cart (Down, _, _)), _, _, _)
    //    | (_, Some (Cart _), _, _)
    //    | (_, _, Some (Cart (Right, _, _)), _)
    //    |  -> Track t |> Some
    //    | _ -> Track t |> Some
    //| Some (Track (StraightTrack Horizontal)) ->
    //    match (left, right) with
    //    | (Some (Cart (Right, _, _)), Some (Cart (Left, _, _))) -> Track (StraightTrack Horizontal) |> Some
    //    | (Some (Cart (Right, td, t)), _) -> Cart (Right, td, StraightTrack Horizontal) |> Some
    //    | (_, Some (Cart (Left, td, t))) -> Cart (Left, td, StraightTrack Horizontal) |> Some
    //    | _ -> place
    //| Some (Track (StraightTrack Vertical)) ->
    //    match (up, down) with
    //    | (Some (Cart (Down, _, _)), Some (Cart (Up, _, _))) -> Track (StraightTrack Vertical) |> Some
    //    | (Some (Cart (Down, td, t)), _) -> Cart (Down, td, StraightTrack Vertical) |> Some
    //    | (_, Some (Cart (Up, td, t))) -> Cart (Up, td, StraightTrack Vertical) |> Some
    //    | _ -> place
    //| Some (Track Intersection) ->
    //    match (up, down, left, right) with
    //    | _ when List.filter id [directionEq up Down; directionEq down Up; directionEq left Right; directionEq right Left] |> List.length > 1 -> Track Intersection |> Some
    //    | (Some (Cart (Down, td, _)), _, _, _) -> turnCart (Cart (Down, td, Intersection)) |> Some
    //    | (_, Some (Cart (Up, td, _)), _, _) -> turnCart (Cart (Up, td, Intersection)) |> Some
    //    | (_, _, Some (Cart (Right, td, _)), _) -> turnCart (Cart (Right, td, Intersection)) |> Some
    //    | (_, _, _, Some (Cart (Left, td, _))) -> turnCart (Cart (Left, td, Intersection)) |> Some
    //    | _ -> Track Intersection |> Some
    //| Some (Track (CurvedTrack FSlash)) ->
    //    match (up, down, left, right) with
    //    | (Some (Cart (Down, _, _)), _, Some (Cart (Right, _, _)), _)
    //    | (_, Some (Cart (Up, _, _)), _, Some (Cart (Left, _, _))) -> Track (CurvedTrack FSlash) |> Some
    //    | (Some (Cart (Down, td, _)), _, _, _) -> Cart (Left, td, CurvedTrack FSlash) |> Some
    //    | (_, Some (Cart (Up, td, _)), _, _) -> Cart (Right, td, CurvedTrack FSlash) |> Some
    //    | (_, _, Some (Cart (Right, td, _)), _) -> Cart (Up, td, CurvedTrack FSlash) |> Some
    //    | (_, _, _, Some (Cart (Left, td, _))) -> Cart (Down, td, CurvedTrack FSlash) |> Some
    //    | _ -> Track (CurvedTrack FSlash) |> Some
    //| Some (Track (CurvedTrack BSlash)) ->
    //    match (up, down, left, right) with
    //    | (Some (Cart (Down, _, _)), _, _, Some (Cart (Left, _, _)))
    //    | (_, Some (Cart (Up, _, _)), Some (Cart (Right, _, _)), _) -> Track (CurvedTrack BSlash) |> Some
    //    | (Some (Cart (Down, td, _)), _, _, _) -> Cart (Right, td, CurvedTrack BSlash) |> Some
    //    | (_, Some (Cart (Up, td, _)), _, _) -> Cart (Left, td, CurvedTrack BSlash) |> Some
    //    | (_, _, Some (Cart (Right, td, _)), _) -> Cart (Down, td, CurvedTrack BSlash) |> Some
    //    | (_, _, _, Some (Cart (Left, td, _))) -> Cart (Up, td, CurvedTrack BSlash) |> Some
    //    | _ -> Track (CurvedTrack BSlash) |> Some

let updateRow (tracks:Place option [][]) y row =
    Array.iteri (updatePlace tracks y) row

let tick (tracks:Place option [][]) =
    Array.iteri (updateRow tracks) tracks
    tracks

let findSome = Array.reduce (fun x r ->
    match (r, x) with
    | (Some r, _) -> Some r
    | (None, Some x) -> Some x
    | (None, None) -> None)

let getCartPos tracks =
    Array.mapi (fun y row ->
        Array.mapi (fun x place ->
            match place with
            | Some (Cart _) -> Some (x, y)
            | _ -> None)
            row
            |> findSome)
        tracks
        |> findSome

let checkCartsLeft tracks =
    Array.map
        (fun row ->
            Array.filter
                (fun place ->
                    match place with
                    | Some (Cart _) -> true
                    | _ -> false)
                row
            |> Array.length)
        tracks
        |> Array.sum

let writeCharacter (c:char) x y color =
    Console.SetCursorPosition(x, y)
    Console.ForegroundColor <- color
    Console.Write(c)

let rec run (tracks:Place option [][]) =
    let cartsLeft = checkCartsLeft tracks
    if cartsLeft % 2 = 0 then
        Some (-1, -1)
    else if cartsLeft = 1 then
        getCartPos tracks
    else if cartsLeft = 0 then
        Some (-1, -1)
    else run (tick tracks)

[<EntryPoint>]
let main argv =
    let lines = readLines argv.[0]
    let tracks = Array.create (Seq.length lines) Array.empty

    Console.Clear()

    Seq.iteri (fun i line -> tracks.[i] <- parseInput line) lines

    run tracks |> printfn "%A"

    0
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

let updatePlace (tracks:Place option [][]) y x (place:Place option) : Place option =
    let up = if y > 0 && x < tracks.[y - 1].Length then tracks.[y - 1].[x] else None
    let down = if y < tracks.Length - 1 && x < tracks.[y + 1].Length then tracks.[y + 1].[x] else None
    let left = if x > 0 then tracks.[y].[x - 1] else None
    let right = if x < tracks.[y].Length - 1 then tracks.[y].[x + 1] else None
    match place with
    | None -> place
    | Some (Cart (_, _, t)) -> Track t |> Some
    | Some (Track (StraightTrack Horizontal)) ->
        match (left, right) with
        | (Some (Cart (Right, _, _)), Some (Cart (Left, _, _))) -> Track (StraightTrack Horizontal) |> Some
        | (Some (Cart (Right, td, t)), _) -> Cart (Right, td, StraightTrack Horizontal) |> Some
        | (_, Some (Cart (Left, td, t))) -> Cart (Left, td, StraightTrack Horizontal) |> Some
        | _ -> place
    | Some (Track (StraightTrack Vertical)) ->
        match (up, down) with
        | (Some (Cart (Down, _, _)), Some (Cart (Up, _, _))) -> Track (StraightTrack Vertical) |> Some
        | (Some (Cart (Down, td, t)), _) -> Cart (Down, td, StraightTrack Vertical) |> Some
        | (_, Some (Cart (Up, td, t))) -> Cart (Up, td, StraightTrack Vertical) |> Some
        | _ -> place
    | Some (Track Intersection) ->
        match (up, down, left, right) with
        | (Some (Cart (Down, td, _)), Some (Track _), Some (Track _), Some (Track _)) -> turnCart (Cart (Down, td, Intersection)) |> Some
        | (Some (Track _), Some (Cart (Up, td, _)), Some (Track _), Some (Track _)) -> turnCart (Cart (Up, td, Intersection)) |> Some
        | (Some (Track _), Some (Track _), Some (Cart (Right, td, _)), Some (Track _)) -> turnCart (Cart (Right, td, Intersection)) |> Some
        | (Some (Track _), Some (Track _), Some (Track _), Some (Cart (Left, td, _))) -> turnCart (Cart (Left, td, Intersection)) |> Some
        | _ when List.filter id [directionEq up Down; directionEq down Up; directionEq left Right; directionEq right Left] |> List.length > 1 -> Track Intersection |> Some
        | _ -> Track Intersection |> Some
    | Some (Track (CurvedTrack FSlash)) ->
        match (up, down, left, right) with
        | (Some (Cart (Down, td, _)), _, Some (Track _), _) -> Cart (Left, td, CurvedTrack FSlash) |> Some
        | (_, Some (Cart (Up, td, _)), _, Some (Track _)) -> Cart (Right, td, CurvedTrack FSlash) |> Some
        | (Some (Track _), _, Some (Cart (Right, td, _)), _) -> Cart (Up, td, CurvedTrack FSlash) |> Some
        | (_, Some (Track _), _, Some (Cart (Left, td, _))) -> Cart (Down, td, CurvedTrack FSlash) |> Some
        | (Some (Track _), _, _, _)
        | (_, Some (Track _), _, _)
        | (_, _, Some (Track _), _)
        | (_, _, _, Some (Track _)) -> Track (CurvedTrack FSlash) |> Some
        | _ -> Track (CurvedTrack FSlash) |> Some
    | Some (Track (CurvedTrack BSlash)) ->
        match (up, down, left, right) with
        | (Some (Cart (Down, td, _)), _, _, Some (Track _)) -> Cart (Right, td, CurvedTrack BSlash) |> Some
        | (_, Some (Cart (Up, td, _)), Some (Track _), _) -> Cart (Left, td, CurvedTrack BSlash) |> Some
        | (_, Some (Track _), Some (Cart (Right, td, _)), _) -> Cart (Down, td, CurvedTrack BSlash) |> Some
        | (Some (Track _), _, _, Some (Cart (Left, td, _))) -> Cart (Up, td, CurvedTrack BSlash) |> Some
        | (Some (Track _), _, _, _)
        | (_, Some (Track _), _, _)
        | (_, _, Some (Track _), _)
        | (_, _, _, Some (Track _)) -> Track (CurvedTrack BSlash) |> Some
        | _ -> Track (CurvedTrack BSlash) |> Some

let updateRow (tracks:Place option [][]) y row =
    Array.mapi (updatePlace tracks y) row

let tick (tracks:Place option [][]) =
    Array.mapi (updateRow tracks) tracks

let findSome = Array.reduce (fun x r ->
    match (r, x) with
    | (Some r, _) -> Some r
    | (None, Some x) -> Some x
    | (None, None) -> None)

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

let printTracks tracks =
    Console.CursorVisible <- false
    Array.iteri (fun y row ->
        Array.iteri (fun x place ->
            match place with
            | Some (Track (StraightTrack Horizontal)) -> writeCharacter '-' x y ConsoleColor.Gray
            | Some (Track (StraightTrack Vertical)) -> writeCharacter '|' x y ConsoleColor.Gray
            | Some (Track (CurvedTrack FSlash)) -> writeCharacter '/' x y ConsoleColor.Gray
            | Some (Track (CurvedTrack BSlash)) -> writeCharacter '\\' x y ConsoleColor.Gray
            | Some (Track Intersection) -> writeCharacter '+' x y ConsoleColor.Gray
            | Some (Cart (Left, _, _)) -> writeCharacter '<' x y ConsoleColor.Blue
            | Some (Cart (Up, _, _)) -> writeCharacter '^' x y ConsoleColor.Blue
            | Some (Cart (Right, _, _)) -> writeCharacter '>' x y ConsoleColor.Blue
            | Some (Cart (Down, _, _)) -> writeCharacter 'v' x y ConsoleColor.Blue
            | _ -> writeCharacter ' ' x y ConsoleColor.Gray)
            row
        printfn "")
        tracks
    printfn ""

let rec run (tracks:Place option [][]) =
    if checkCartsLeft tracks == 1 then
        getCartPos tracks
    else run (tick tracks)

[<EntryPoint>]
let main argv =
    let lines = readLines argv.[0]
    let tracks = Array.create (Seq.length lines) Array.empty

    Seq.iteri (fun i line -> tracks.[i] <- parseInput line) lines

    run tracks |> printfn "%A"

    0
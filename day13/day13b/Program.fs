open System

type Direction = Up | Right | Down | Left
type TurnDirection = TurnLeft | GoStraight | TurnRight

type StraightTrackType = Vertical | Horizontal
type CurvedTrackType = FSlash | BSlash
type Track = StraightTrack of StraightTrackType | CurvedTrack of CurvedTrackType | Intersection

type TrackBeneath = Track
type Place = Track of Track | Cart of (Direction * TurnDirection * TrackBeneath * bool)

let readLines filePath = IO.File.ReadLines(filePath)

let charToPlace c =
    match c with
    | '-' -> Track (StraightTrack Horizontal) |> Some
    | '|' -> Track (StraightTrack Vertical) |> Some
    | '\\' -> Track (CurvedTrack BSlash) |> Some
    | '/' -> Track (CurvedTrack FSlash) |> Some
    | '+' -> Track Intersection |> Some
    | '^' -> Cart (Up, TurnLeft, StraightTrack Vertical, false) |> Some
    | '>' -> Cart (Right, TurnLeft, StraightTrack Horizontal, false) |> Some
    | 'v' -> Cart (Down, TurnLeft, StraightTrack Vertical, false) |> Some
    | '<' -> Cart (Left, TurnLeft, StraightTrack Horizontal, false) |> Some
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
    | Cart (d, GoStraight, t, m) -> Cart (d, TurnRight, t, m)
    | Cart (d, TurnLeft, t, m) -> Cart (turnLeft d, GoStraight, t, m)
    | Cart (d, TurnRight, t, m) -> Cart (turnRight d, TurnLeft, t, m)
    | _ -> place

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
            | Some (Cart (Left, _, _, _)) -> writeCharacter '<' x y ConsoleColor.Blue
            | Some (Cart (Up, _, _, _)) -> writeCharacter '^' x y ConsoleColor.Blue
            | Some (Cart (Right, _, _, _)) -> writeCharacter '>' x y ConsoleColor.Blue
            | Some (Cart (Down, _, _, _)) -> writeCharacter 'v' x y ConsoleColor.Blue
            | _ -> writeCharacter ' ' x y ConsoleColor.Gray)
            row
        printfn "")
        tracks
    printfn ""

let updatePlace (tracks:Place option [][]) y x (place:Place option) : unit =
    match place with
    | Some (Cart (Left, td, t, false)) ->
        match tracks.[y].[x-1] with
        | Some (Cart (_, _, t, _)) -> tracks.[y].[x-1] <- Some (Track t)
        | Some (Track track) ->
            tracks.[y].[x-1] <-
                match track with
                | CurvedTrack FSlash -> Some (Cart (Down, td, CurvedTrack FSlash, true))
                | CurvedTrack BSlash -> Some (Cart (Up, td, CurvedTrack BSlash, true))
                | Intersection -> Some (turnCart (Cart (Left, td, Intersection, true)))
                | _ -> Some (Cart (Left, td, track, true))
        | _ -> ()
        tracks.[y].[x] <- Some (Track t)
    | Some (Cart (Right, td, t, false)) ->
        match tracks.[y].[x+1] with
        | Some (Cart (_, _, t, _)) -> tracks.[y].[x+1] <- Some (Track t)
        | Some (Track track) ->
            tracks.[y].[x+1] <-
                match track with
                | CurvedTrack FSlash -> Some (Cart (Up, td, CurvedTrack FSlash, true))
                | CurvedTrack BSlash -> Some (Cart (Down, td, CurvedTrack BSlash, true))
                | Intersection -> Some (turnCart (Cart (Right, td, Intersection, true)))
                | _ -> Some (Cart (Right, td, track, true))
        | _ -> ()
        tracks.[y].[x] <- Some (Track t)
    | Some (Cart (Up, td, t, false)) ->
        match tracks.[y-1].[x] with
        | Some (Cart (_, _, t, _)) -> tracks.[y-1].[x] <- Some (Track t)
        | Some (Track track) ->
            tracks.[y-1].[x] <-
                match track with
                | CurvedTrack FSlash -> Some (Cart (Right, td, CurvedTrack FSlash, true))
                | CurvedTrack BSlash -> Some (Cart (Left, td, CurvedTrack BSlash, true))
                | Intersection -> Some (turnCart (Cart (Up, td, Intersection, true)))
                | _ -> Some (Cart (Up, td, track, true))
        | _ -> ()
        tracks.[y].[x] <- Some (Track t)
    | Some (Cart (Down, td, t, false)) ->
        match tracks.[y+1].[x] with
        | Some (Cart (_, _, t, _)) -> tracks.[y+1].[x] <- Some (Track t)
        | Some (Track track) ->
            tracks.[y+1].[x] <-
                match track with
                | CurvedTrack FSlash -> Some (Cart (Left, td, CurvedTrack FSlash, true))
                | CurvedTrack BSlash -> Some (Cart (Right, td, CurvedTrack BSlash, true))
                | Intersection -> Some (turnCart (Cart (Down, td, Intersection, true)))
                | _ -> Some (Cart (Down, td, track, true))
        | _ -> ()
        tracks.[y].[x] <- Some (Track t)
    | _ -> ()

let updateRow (tracks:Place option [][]) y row =
    Array.iteri (updatePlace tracks y) row

let tick (tracks:Place option [][]) =
    Array.iteri (updateRow tracks) tracks
    Array.map
        (fun row ->
            Array.map
                (fun place ->
                    match place with
                    | Some (Cart (d, td, t, _)) -> Some (Cart (d, td, t, false))
                    | p -> p)
                row)
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

    Seq.iteri (fun i line -> tracks.[i] <- parseInput line) lines

    run tracks |> printfn "%A"

    0
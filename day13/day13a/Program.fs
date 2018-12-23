open System

type Direction = Up | Right | Down | Left

type StraightTrackType = Vertical | Horizontal
type CurvedTrackType = FSlash | BSlash
type Track = StraightTrack of StraightTrackType | CurvedTrack of CurvedTrackType | Intersection

type Place = Track of Track | Cart of Direction

let readLines filePath = IO.File.ReadLines(filePath)

let charToPlace c =
    match c with
    | '-' -> Track (StraightTrack Horizontal) |> Some
    | '|' -> Track (StraightTrack Vertical) |> Some
    | '\\' -> Track (CurvedTrack BSlash) |> Some
    | '/' -> Track (CurvedTrack FSlash) |> Some
    | '+' -> Track Intersection |> Some
    | '^' -> Cart Up |> Some
    | '>' -> Cart Right |> Some
    | 'v' -> Cart Down |> Some
    | '<' -> Cart Left |> Some
    | _   -> None

let parseInput s = Array.map charToPlace (Array.ofSeq s)

[<EntryPoint>]
let main argv =
    let lines = readLines argv.[0]
    let tracks = Array.create (Seq.length lines) Array.empty
    Seq.iteri (fun i line -> tracks.[i] <- parseInput line) lines
    printfn "%A" tracks
    0
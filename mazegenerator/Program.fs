/// A maze generator and solver created by Connor Bradley, 2017
open System

let rnd = Random()
let Height = 10
let Width = 10


//Used to store details about the individual blocks of the maze
type MazePiece = class
    val X : int
    val Y : int
    val IsBlock : int

    new (x, y) =
        { X = x; Y = y; IsBlock = if ((x = 0) && (y =0)) || ((x = Height-1) && (y = Width-1)) then 0 else (if rnd.Next(0, 10) > 7 then 1 else 0)}

    member x.Details = 
        printfn "Piece at %i , %i , is a block? : %i" x.X x.Y x.IsBlock
end

type Solveable = 
    | Yes = 1
    | No = 0

// Generates all the maze stuff 
let GenerateMazeLine() h = [for i=0 to Width-1 do yield (new MazePiece(h, i))]

let GenerateMaze = [for i=0 to Height-1 do yield (GenerateMazeLine() i)]

let extractAndPrintMaze() (mazeLine : MazePiece List) = 
    printfn "" //create new line
    mazeLine |> List.iter (fun x -> printf "%s" (if (x.IsBlock = 1) then "█" else "░" ))



//Navigates all the maze stuff 
printf "--MAZE--"
GenerateMaze |> List.iter (fun x -> (extractAndPrintMaze() x)) 
printfn "\n--------"

let isRightOkay() (curMazePiece : MazePiece) (maze: MazePiece List List) = 
    //printfn "R: y=%i x=%i" curMazePiece.X curMazePiece.Y
    let curX = curMazePiece.X
    let curY = curMazePiece.Y
    let rightPiece = maze |> List.item curX
                          |> List.item (curY + 1)
    rightPiece

let isDownOkay() (curMazePiece : MazePiece) (maze: MazePiece List List) = 
    //printfn "D: y=%i x=%i" curMazePiece.X curMazePiece.Y
    let curX = curMazePiece.X
    let curY = curMazePiece.Y
    let downPiece = maze |> List.item (curX + 1)
                         |> List.item curY
    downPiece


let isLeftOkay() (curMazePiece : MazePiece) (maze: MazePiece List List) = 
    //printfn "L: y=%i x=%i" curMazePiece.X curMazePiece.Y
    let curX = curMazePiece.X
    let curY = curMazePiece.Y
    let leftPiece = maze |> List.item curX
                         |> List.item (curY - 1)
    leftPiece

let isUpOkay() (curMazePiece : MazePiece) (maze: MazePiece List List) = 
    //printfn "U: y=%i x=%i" curMazePiece.X curMazePiece.Y
    let curX = curMazePiece.X
    let curY = curMazePiece.Y
    let upPiece = maze |> List.item (curX - 1)
                       |> List.item curY
    upPiece

let mutable counter = 0

let rec FigureOutMaze() (currPiece : MazePiece) =
    if counter < 50 then 
        counter <- counter+1 
        if (currPiece.Y = Width-1 && currPiece.X = Height-1) then
            printf "Done"
        elif  (if currPiece.Y <> (Width-1) then (isRightOkay() currPiece GenerateMaze).IsBlock else 1) = 0 then
            printf "Right -> "
            FigureOutMaze() (isRightOkay() currPiece GenerateMaze)  
        elif  (if currPiece.X <> (Height-1) then (isDownOkay() currPiece GenerateMaze).IsBlock else 1) = 0 then
            printf "Down -> "
            FigureOutMaze() (isDownOkay() currPiece GenerateMaze)
         elif (if currPiece.Y > 0 then (isLeftOkay() currPiece GenerateMaze).IsBlock else 1) = 0 then
            printf "Left -> "
            FigureOutMaze() (isLeftOkay() currPiece GenerateMaze)  
        elif (if currPiece.X > 0 then (isUpOkay() currPiece GenerateMaze).IsBlock else 1) = 0 then
            printf "Up ->"
            FigureOutMaze() (isUpOkay() currPiece GenerateMaze)  
        else
            printf "Can't Move"
    else printfn "\nI give up, its probably unsolveable, I got to y=%i x=%i" currPiece.X currPiece.Y


let startingPiece = GenerateMaze |> List.item 0
                                 |> List.item 0
FigureOutMaze() startingPiece

Console.ReadKey() |> ignore

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

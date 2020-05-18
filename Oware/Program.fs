module Oware

type StartingPosition =
    | South
    | North

type Board = 
    { House : int*int*int*int*int*int*int*int*int*int*int*int
      Turn : StartingPosition
      Score : int*int} // 0 - South && 1 - North


let getSeeds n board = 
    let {House = (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)} = board
    match n with
    | 1 -> h1 | 2 -> h2 | 3 -> h3 | 4 -> h4 | 5 -> h5 | 6 -> h6 | 7 -> h7 | 8 -> h8 | 9 -> h9 | 10 -> h10 | 11 -> h11 | 12 -> h12 | _ -> failwith "Incorrect n house" 

let printBoard board =
    let rec printer i board=
        match (i < 13) with
        | true -> 
            printer (i+1) board
            printfn "House %d: %d" i (getSeeds i board)
        | false -> ()
    printer 1 board

let setHouseToZero board n =
    let {House=(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)} = board
    match n with
    | 1 -> (0,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)
    | 2 -> (h1,0,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)
    | 3 -> (h1,h2,0,h4,h5,h6,h7,h8,h9,h10,h11,h12)
    | 4 -> (h1,h2,h3,0,h5,h6,h7,h8,h9,h10,h11,h12)
    | 5 -> (h1,h2,h3,h4,0,h6,h7,h8,h9,h10,h11,h12)
    | 6 -> (h1,h2,h3,h4,h5,0,h7,h8,h9,h10,h11,h12)
    | 7 -> (h1,h2,h3,h4,h5,h6,0,h8,h9,h10,h11,h12)
    | 8 -> (h1,h2,h3,h4,h5,h6,h7,0,h9,h10,h11,h12)
    | 9 -> (h1,h2,h3,h4,h5,h6,h7,h8,0,h10,h11,h12)
    | 10 -> (h1,h2,h3,h4,h5,h6,h7,h8,h9,0,h11,h12)
    | 11 -> (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,0,h12)
    | 12 -> (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,0)
    | _ -> failwith "error n out of bounds"

let addTokenToHouse board n = 
    let {House=(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)} = board
    match n with
    | 1 -> (h1+1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)
    | 2 -> (h1,h2+1,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)
    | 3 -> (h1,h2,h3+1,h4,h5,h6,h7,h8,h9,h10,h11,h12)
    | 4 -> (h1,h2,h3,h4+1,h5,h6,h7,h8,h9,h10,h11,h12)
    | 5 -> (h1,h2,h3,h4,h5+1,h6,h7,h8,h9,h10,h11,h12)
    | 6 -> (h1,h2,h3,h4,h5,h6+1,h7,h8,h9,h10,h11,h12)
    | 7 -> (h1,h2,h3,h4,h5,h6,h7+1,h8,h9,h10,h11,h12)
    | 8 -> (h1,h2,h3,h4,h5,h6,h7,h8+1,h9,h10,h11,h12)
    | 9 -> (h1,h2,h3,h4,h5,h6,h7,h8,h9+1,h10,h11,h12)
    | 10 -> (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10+1,h11,h12)
    | 11 -> (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11+1,h12)
    | 12 -> (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12+1)
    | _ -> failwith "error n out of bounds"

let changePlayers board =
    let {House=(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12); Turn=t; Score=(sScore,nScore)} = board
    match (t = South) with
    | true -> {board with Turn=North}
    | false -> {board with Turn=South}

let rec collectTokens board n =
    let {House=(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12); Turn=t; Score=(sScore,nScore)} = board
    match ((getSeeds n board) = 2) || ((getSeeds n board) = 3) with
    | true -> 
        match (t = South) with
        | true -> 
            match ((n >= 7) && (n <= 12)) with
            | true ->
                match (n = 7) with
                | true -> collectTokens {board with House=(setHouseToZero board n); Score=((sScore + (getSeeds n board)), nScore)} n
                | false -> collectTokens {board with House=(setHouseToZero board n); Score=((sScore + (getSeeds n board)), nScore)} (n-1)
            | false -> board
        | false ->
            match ((n >= 1) && (n <= 6)) with
            | true -> 
                match (n = 1) with
                | true -> collectTokens {board with House=(setHouseToZero board n); Score=(sScore, (nScore + (getSeeds n board)))} n
                | false -> collectTokens {board with House=(setHouseToZero board n); Score=(sScore, (nScore + (getSeeds n board)))} (n-1)
            | false -> board
    | false -> board

let useHouse n board =
    let {Turn=t} = board
    let originalHouse = n
    let rec changeHouse n newBoard counter =
        match (counter = 0) with
        | true -> 
            match ((getSeeds n {board with House=newBoard}) = 2) || ((getSeeds n {board with House=newBoard}) = 3) with
            | false -> changePlayers {board with House=newBoard} 
            | true -> changePlayers (collectTokens {board with House=newBoard} n)
        | false ->
            match (originalHouse = n) with
            | true -> changeHouse ((n%12)+1) newBoard counter
            | false ->
                match (counter = 1) with
                | true -> changeHouse n (addTokenToHouse {board with House=newBoard} n) (counter-1)
                | false -> changeHouse ((n%12)+1) (addTokenToHouse {board with House=newBoard} n) (counter-1)

    match (t = South) with
    | true -> 
        match (((n >= 1) && (n <= 6)) && (getSeeds n board) <> 0) with
        | true -> changeHouse ((n%12)+1) (setHouseToZero board n) (getSeeds n board)
        | false -> board
    | false ->
        match (((n >= 7) && (n <= 12)) && (getSeeds n board) <> 0) with
        | true -> changeHouse ((n%12)+1) (setHouseToZero board n) (getSeeds n board)
        | false -> board
    

let start position = 
    match position with
    | South -> {House=(4,4,4,4,4,4,4,4,4,4,4,4); Turn=South; Score=(0,0)}
    | North -> {House=(4,4,4,4,4,4,4,4,4,4,4,4); Turn=North; Score=(0,0)}

let score board = 
    let {Score=(sScore,nScore)} = board
    (sScore,nScore)

let gameState board = 
    let {Turn=t; Score=(sScore,nScore)} = board
    match ((sScore = 24) || (nScore = 24)) with
    | true -> "Game ended in a draw"
    | false ->
        match ((sScore >= 25) || (nScore >= 25)) with
        | true -> 
            match (sScore >= 25) with
            | true -> "South won"
            | false -> "North won"
        | false ->
            match (t=South) with
            | true -> "South's turn"
            | false -> "North's turn"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    
    let playGame numbers =
        let rec play xs game =
            match xs with
            | [] -> game
            | x::xs -> play xs (useHouse x game)
        play numbers (start South)

    playGame [6; 8; 5; 9; 4; 12; 3; 10; 1; 11; 2; 12; 5; 7; 5; 11; 6; 8; 1; 12; 4; 10; 5; 9;
    2; 11; 3; 12; 6; 9; 5; 10; 2; 11; 1; 12; 4; 7; 6; 7; 3; 8; 5; 9; 6; 10; 1; 11;
     2; 12; 3; 8; 4; 9; 1; 10; 2; 11; 3; 12; 1] |> printBoard

    //board |> printBoard
    0 // return an integer exit code

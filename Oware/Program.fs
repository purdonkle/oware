module Oware

type StartingPosition =
    | South
    | North

type Board = 
    { House : int*int*int*int*int*int*int*int*int*int*int*int
      Turn : StartingPosition
      Score : int*int} // (south, north)

// return number of tokens at house n on board
// input : n - int, board - Board
// return : int
let getSeeds n board = 
    let {House = (h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)} = board
    match n with
    | 1 -> h1 | 2 -> h2 | 3 -> h3 | 4 -> h4 | 5 -> h5 | 6 -> h6 | 7 -> h7 | 8 -> h8 | 9 -> h9 | 10 -> h10 | 11 -> h11 | 12 -> h12 | _ -> failwith "Incorrect n house" 

// set number of tokens at house n to zero
// input : board - Board, n - int
// return : int*int*int*int*int*int*int*int*int*int*int*int
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

// add a single token at house n
// input : board - Board, n - int
// return : int*int*int*int*int*int*int*int*int*int*int*int
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

// invert the current player of board
// input : board - Board
// return : Board
let changePlayers board =
    let {House=(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12); Turn=t; Score=(sScore,nScore)} = board
    match (t = South) with
    | true -> {board with Turn=North}
    | false -> {board with Turn=South}

// collect tokens for scoring where tokens are 2 or 3 and according to rules
// input : board - Board, n - int
// return Board
let rec collectTokens board n =
    let {House=(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12); Turn=t; Score=(sScore,nScore)} = board
    match ((getSeeds n board) = 2) || ((getSeeds n board) = 3) with // tokens at house are 2 or 3 
    | true -> 
        match (t = South) with // South is the player
        | true -> 
            match ((n >= 7) && (n <= 12)) with // collecting from north opponents houses only
            | true ->
                match (n = 7) with // is the house contiguous?
                | true -> // yes house is contiguous
                    let futureBoard = collectTokens {board with House=(setHouseToZero board n); Score=((sScore + (getSeeds n board)), nScore)} (n) // save the next recursion call to futureBoard
                    let {House=(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)} = futureBoard
                    match ((h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12) = (h1,h2,h3,h4,h5,h6,0,0,0,0,0,0)) with // will the following recursion call leave the opposite player with no tokens?
                    | true -> 
                        match ((h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12) = (0,0,0,0,0,0,0,0,0,0,0,0)) with // is this a case of a 24 - 24 draw?
                        | true ->
                            collectTokens {board with House=(setHouseToZero board n); Score=((sScore + (getSeeds n board)), nScore)} n // yes case of 24 - 24 draw - draw last tokens
                        | false -> board // no not a 24 - 24 draw invalid move
                    | false -> collectTokens {board with House=(setHouseToZero board n); Score=((sScore + (getSeeds n board)), nScore)} n // return the current board because futureBoard leaves the player with no tokens
                | false -> collectTokens {board with House=(setHouseToZero board n); Score=((sScore + (getSeeds n board)), nScore)} (n-1) // house is not contiguous 
            | false -> board // cannot collect own house tokens
        | false -> // north is the player
            match ((n >= 1) && (n <= 6)) with // collecting from south opponents houses only
            | true -> 
                match (n = 1) with // is the house contiguous?
                | true -> // yes house is contiguous
                    let futureBoard = collectTokens {board with House=(setHouseToZero board n); Score=((sScore + (getSeeds n board)), nScore)} (n) // save the next recursion call to futureBoard
                    let {House=(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)} = futureBoard
                    match ((h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12) = (0,0,0,0,0,0,h7,h8,h9,h10,h11,h12)) with // will the following recursion call leave the opposite player with no tokens?
                    | true -> 
                        match ((h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12) = (0,0,0,0,0,0,0,0,0,0,0,0)) with // is this a case of a 24 - 24 draw?
                        | true -> 
                            collectTokens {board with House=(setHouseToZero board n); Score=(sScore, (nScore + (getSeeds n board)))} n // yes case of 24 - 24 draw - draw last tokens
                        | false -> board // no not a 24 - 24 draw invalid move
                    | false -> collectTokens {board with House=(setHouseToZero board n); Score=(sScore, (nScore + (getSeeds n board)))} n // return the current board because futureBoard leaves the player with no tokens
                | false ->  collectTokens {board with House=(setHouseToZero board n); Score=(sScore, (nScore + (getSeeds n board)))} (n-1) // house is not contiguous 
            | false -> board // cannot collect own house tokens
    | false -> board // tokens at house does not equal 2 or 3

// distribute the tokens clockwise across the board according to the rules
// input : n - int, board - Board
// return : Board
let useHouse n board = 

    // recursive loop distributing the tokens from the chosen house
    // input : pos - int, houses - int*int*int*int*int*int*int*int*int*int*int*int, counter - int
    // return : Board
    let rec changeHouse pos houses counter =
        match (counter = 0) with // decrement until counter = 0
        | true -> // counter = 0
            match ((getSeeds pos {board with House=houses}) = 2) || ((getSeeds pos {board with House=houses}) = 3) with // does the last distributed token make a house or 2 or 3?
            | false -> changePlayers {board with House=houses} // no it is not a house of 2 or 3
            | true -> changePlayers (collectTokens {board with House=houses} pos) // yes it is a house of 2 or 3 so we collect the tokens
        | false -> // counter <> 0
            match (n = pos) with // are we trying to distribute to the original house?
            | true -> changeHouse ((pos%12)+1) houses counter // yes we are - skip this house
            | false -> // no we are not 
                match (counter = 1) with // is this the second last token we are placing down?
                | true -> changeHouse pos (addTokenToHouse {board with House=houses} pos) (counter-1) // yes it is the last token so we dont increase pos 
                | false -> changeHouse ((pos%12)+1) (addTokenToHouse {board with House=houses} pos) (counter-1) // no it is not so we increase pos to move to the next house

    let {Turn=t} = board
    match (t = South) with // is it south's turn?
    | true -> // south's turn
        match (((n >= 1) && (n <= 6)) && (getSeeds n board) <> 0) with // is south moving south's tokens and does south have tokens to place?
        | true -> // yes and yes makes a yes
            let futureBoard = changeHouse ((n%12)+1) (setHouseToZero board n) (getSeeds n board) // save south's next recursion call to futureBoard 
            let {House=(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)} = futureBoard
            match ((h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12) = (h1,h2,h3,h4,h5,h6,0,0,0,0,0,0)) with // is south's next move going to take all of oppnents tokens?
            | true -> // yes south's next turn will take all the opponents pieces
                match ((h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12) = (0,0,0,0,0,0,0,0,0,0,0,0)) with // is the futureBoard a 24 - 24 draw case?
                | true -> changeHouse ((n%12)+1) (setHouseToZero board n) (getSeeds n board) // yes 24 - 24 draw case so we allow the distribution
                | false -> board // not a 24 - 24 draw case invalid move
            | false -> changeHouse ((n%12)+1) (setHouseToZero board n) (getSeeds n board) // no south's turn does not take all the opponents pieces valid move
        | false -> board // no and yes or yes and no makes a no
    | false -> // north's turn
        match (((n >= 7) && (n <= 12)) && (getSeeds n board) <> 0) with // is north moving north's tokens and does north have tokens to place?
        | true -> // yes and yes makes a yes
            let futureBoard = changeHouse ((n%12)+1) (setHouseToZero board n) (getSeeds n board) // save north's next recursion call to futureBoard 
            let {House=(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12)} = futureBoard
            match ((h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12) = (0,0,0,0,0,0,h7,h8,h9,h10,h11,h12)) with // is north's next move going to take all of oppnents tokens?
            | true -> // yes north's next turn will take all the opponents pieces
                match ((h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12) = (0,0,0,0,0,0,0,0,0,0,0,0)) with // is the futureBoard a 24 - 24 draw case?
                | true -> changeHouse ((n%12)+1) (setHouseToZero board n) (getSeeds n board) // yes 24 - 24 draw case so we allow the distribution
                | false -> board // not a 24 - 24 draw case invalid move
            | false -> changeHouse ((n%12)+1) (setHouseToZero board n) (getSeeds n board) // no north's turn does not take all the opponents pieces valid move
        | false -> board // no and yes or yes and no makes a no
    

// creates the starting board
// input : position - StartingPosition
// output : Board
let start position = 
    match position with
    | South -> {House=(4,4,4,4,4,4,4,4,4,4,4,4); Turn=South; Score=(0,0)} // south started
    | North -> {House=(4,4,4,4,4,4,4,4,4,4,4,4); Turn=North; Score=(0,0)} // north started

// returns the score from a board
// input : board - Board
// output : (int*int)
let score board = 
    let {Score=(sScore,nScore)} = board
    (sScore,nScore)

// decides who wins accoring to the scores
// input : board - Board
// output : String
let gameState board = 
    let {Turn=t; Score=(sScore,nScore)} = board
    match ((sScore = 24) || (nScore = 24)) with // draw?
    | true -> "Game ended in a draw" // yes
    | false -> // no
        match ((sScore >= 25) || (nScore >= 25)) with // winner?
        | true -> 
            match (sScore >= 25) with
            | true -> "South won" // south wins
            | false -> "North won" // north wins
        | false ->
            match (t=South) with // must still be continuing
            | true -> "South's turn" // souths turn
            | false -> "North's turn" // norths turn

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code

module TicTacToe where
    
-- TIC TAC TOE Game Logic (pure)
winningCells = [[0,1,2], [3,4,5], [6,7,8], [0,3,6], [1,4,7], [2,5,8], [0,4,8], [2,4,6]]
hasSameValues s v                = all (==v) s
checkWinner  b player            = any (==True) $ map (\s-> hasSameValues (getWinCellValues b s) player) winningCells
checkFull    b                   = all (>0) b
getWinCellValues b  pos          = map (\p-> b!!p) pos
makeMove b player pos            = s1 ++ [player] ++ (tail s2) where (s1,s2) = splitAt pos b
isValidMove (Board b player) pos = (pos <9) && (b!!pos)==0
nextPlayer player                = if player==1 then 2 else 1

data Board = Board [Int] Int
data GameState = Initial Board| GameOver Board String| NextMove Board String | InvalidMove Board String

getBoard (Initial brd) = brd
getBoard (NextMove brd _) = brd
getBoard (GameOver brd _) = brd
getBoard (InvalidMove brd _) = brd

getMessage gstate = case gstate of
    Initial brd            -> "Game Start. Player 1" 
    InvalidMove brd errMsg -> errMsg
    GameOver brd winMsg    -> winMsg
    NextMove brd  msg      -> msg

processMove brd pos = 
    if isValidMove brd pos then
        let (Board b player) = brd
            newBoard = makeMove b player pos
        in  if checkWinner newBoard player then
                GameOver (Board newBoard player) ("Winner "++(show player))
            else  if checkFull newBoard  then
                    GameOver (Board newBoard player) ("All Squares played. Game Stuck !!")
                  else
                    let next = (nextPlayer player)
                    in NextMove (Board newBoard next) ("Next Player : "++(show next)) 
    else
        InvalidMove brd "Already Played in this Square. Try Another!!"

ticTacToe bId gstate = 
    if bId < 0  then gstate else
    case gstate of
        Initial brd        -> processMove brd bId
        InvalidMove brd _  -> processMove brd bId
        GameOver brd _     -> Initial firstMove
        NextMove newBoard _-> processMove newBoard bId

board = take 9 $ repeat 0
firstMove = Board board 1

-- Text Based Game Version ==
printBoard b =
    let (r1,r) = splitAt 3 b
        (r2,r3) = splitAt 3 r
    in do print r1
          print r2
          print r3

runTui gstate = do
    let (Board brd player) = getBoard gstate
    printBoard brd
    print (getMessage gstate)
    putStrLn $ "Enter position to play (0-8) for player "++(show player)
    s <- getLine
    let pos =  read s::Int
    do
        newState <- return (ticTacToe pos gstate)
        runTui newState

testTui = runTui (Initial firstMove)

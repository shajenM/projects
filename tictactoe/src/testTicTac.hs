import Hudak
import Hudak.UserEvent as UE
import TicTacToe

-- Each square on Tic Tac Toe board is made with a button of intial color Brown.
-- game state
gameState = constB (Initial firstMove) `switch` onLeftButtonClick =>> playGame where
    onLeftButtonClick = lbp `snapshot` gameState
    playGame (loc, gstate) = constB (ticTacToe  (getButtonId loc) gstate)
    getButtonId loc = 
        let matches = filter (\x->isInButton loc (fst x) buttonSize) buttonLocsWithId 
        in  case matches of
                [] -> -1
                ((p,i):ms) -> i

-- game messages 
gameMsg = lift1 getMessage gameState
gameMsgDisplay = txtMsgAt (-4) (-2.5) font30B peach (lift1 strToTxt gameMsg)

-- make 9 buttons
buttonSize = 1.0
buttonGap  = 0.05
firstButtonX = (-1.0)
firstButtonY = (-1.0)
secondButtonX = firstButtonX + buttonSize + buttonGap
secondButtonY = firstButtonY + buttonSize + buttonGap

brow = take 3 [firstButtonX, secondButtonX.. ]
bcol = take 3 [firstButtonY, secondButtonY.. ]
buttonLocs = do
    x <- bcol
    y <- brow
    return (x,y)
buttonLocsWithId = zip buttonLocs [0..]

makeButton (loc, bId) = buttonPic loc buttonSize bColor where
    bColor = lift2 getColor (constB bId) gameState

-- The Color is changed to Yellow or Blue when user update a square
getColor bId gs = let (Board board p) = getBoard gs in gameColor (board!!bId)

gameColor 0 = Brown
gameColor 1 = Yellow
gameColor 2 = Blue

buttons = map makeButton buttonLocsWithId

-- boader arround tic tac toe board
border = paintedPicture brown  p3 where
    p1 = (constB (firstButtonX-buttonGap), constB (firstButtonY-buttonGap))
    p2 = constB ((buttonSize+buttonGap)*3+buttonGap)
    p3 = translate p1 (rect  p2 p2)
-- combine all squares using `over` 
ticTacToeBoard = foldr over (constB EmptyPic) buttons
bbs =  ticTacToeBoard `over` border `over` gameMsgDisplay

-- Run game
test = testReact  "Tic Tac Toe" bbs resources

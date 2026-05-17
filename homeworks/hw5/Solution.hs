{-# LANGUAGE TupleSections #-}
import Control.Monad.State
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Map (Map)


data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
    deriving (Show)

execInstr :: Instr -> State[Int]()
execInstr (PUSH x) = modify (x:)

execInstr POP = do
    stack <- get
    case stack of
        []     -> return ()
        (_:xs) -> put xs

execInstr DUP = do
    stack <- get
    case stack of
        []     -> return ()
        (x:xs) -> put (x:x:xs)

execInstr SWAP = do
    stack <- get
    case stack of
        (x:y:xs) -> put (y:x:xs)
        _        -> return ()

execInstr ADD = do
    stack <- get
    case stack of
        (x:y:xs) -> put ((x + y) : xs)
        _        -> return ()

execInstr MUL = do
    stack <- get
    case stack of
        (x:y:xs) -> put ((x * y) : xs)
        _        -> return ()

execInstr NEG = do
    stack <- get
    case stack of
        (x:xs) -> put ((-x) : xs)
        _      -> return ()

execProg :: [Instr] -> State [Int] ()
execProg [] = return ()
execProg (i:is) = do
    execInstr i
    execProg is

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []

data Expr
    = Num Int
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | Assign String Expr
    | Seq Expr Expr
    deriving (Show)

eval :: Expr -> State (Map String Int) Int
eval (Num x) = return x

eval (Var name) = do
    env <- get
    return (env Map.! name)
eval (Add e1 e2) = do
    x <- eval e1
    y <- eval e2
    return (x + y)

eval (Mul e1 e2) = do
    x <- eval e1
    y <- eval e2
    return (x * y)

eval (Neg e) = do
    x <- eval e
    return (-x)

eval (Assign name e) = do
    value <- eval e
    modify (Map.insert name value)
    return value

eval (Seq e1 e2) = do
    eval e1
    eval e2

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i, j) cache of
        Just value -> return value
        Nothing    -> do
            value <- compute
            modify (Map.insert (i, j) value)
            return value
  where
    compute
        | i == 0 = return j
        | j == 0 = return i
        | xs !! (i - 1) == ys !! (j - 1) =
            editDistM xs ys (i - 1) (j - 1)
        | otherwise = do
            deletion     <- editDistM xs ys (i - 1) j
            insertion    <- editDistM xs ys i (j - 1)
            substitution <- editDistM xs ys (i - 1) (j - 1)
            return (1 + minimum [deletion, insertion, substitution])

editDistance :: String -> String -> Int
editDistance xs ys =
    evalState (editDistM xs ys (length xs) (length ys)) Map.empty

data Location
    = Normal
    | Decision [String]
    | Obstacle Int
    | Treasure Int
    | Trap Int
    | MainTreasure
    deriving (Show)

data GameState = GameState
    { position :: Int
    , energy   :: Int
    , score    :: Int
    , path     :: String
    } deriving (Show)

type Board = Map (String, Int) Location

type AdventureGame a = StateT GameState IO a

board :: Board
board = Map.fromList
    [ (("main", 0), Normal)
    , (("main", 1), Treasure 10)
    , (("main", 2), Decision ["forest", "cave"])
    , (("main", 3), Obstacle 2)
    , (("main", 4), Treasure 20)
    , (("main", 5), MainTreasure)

    , (("forest", 0), Normal)
    , (("forest", 1), Trap 5)
    , (("forest", 2), Treasure 30)
    , (("forest", 3), Obstacle 3)
    , (("forest", 4), MainTreasure)

    , (("cave", 0), Normal)
    , (("cave", 1), Obstacle 4)
    , (("cave", 2), Treasure 50)
    , (("cave", 3), Trap 20)
    , (("cave", 4), MainTreasure)
    ]

movePlayer :: Int -> AdventureGame Int
movePlayer dice = do
    game <- get
    let oldPosition = position game
    let newPosition = oldPosition + dice
    let newEnergy = energy game - dice

    put game
        { position = newPosition
        , energy = newEnergy
        }

    return dice

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
    choice <- lift (getPlayerChoice options)
    game <- get
    put game
        { path = choice
        , position = 0
        }
    return choice

handleLocation :: AdventureGame Bool
handleLocation = do
    game <- get

    let currentPath = path game
    let currentPosition = position game
    let location = Map.findWithDefault Normal (currentPath, currentPosition) board

    case location of
        Normal -> do
            lift (putStrLn "Normal location.")
            return False

        Decision options -> do
            lift (putStrLn "You reached a decision point.")
            makeDecision options
            return False

        Obstacle damage -> do
            lift (putStrLn ("Obstacle! You lose " ++ show damage ++ " energy."))
            modify (\g -> g { energy = energy g - damage })
            return False

        Treasure points -> do
            lift (putStrLn ("Treasure! You gain " ++ show points ++ " points."))
            modify (\g -> g { score = score g + points })
            return False

        Trap points -> do
            lift (putStrLn ("Trap! You lose " ++ show points ++ " points."))
            modify (\g -> g { score = max 0 (score g - points) })
            return False

        MainTreasure -> do
            lift (putStrLn "You reached the main treasure!")
            return True

playTurn :: AdventureGame Bool
playTurn = do
    game <- get

    if energy game <= 0
        then do
            lift (putStrLn "You have no energy left.")
            return True
        else do
            dice <- lift getDiceRoll
            moved <- movePlayer dice
            lift (putStrLn ("You moved " ++ show moved ++ " spaces."))

            finished <- handleLocation

            newGame <- get
            lift (displayGameState newGame)

            if energy newGame <= 0
                then do
                    lift (putStrLn "Game over. You ran out of energy.")
                    return True
                else return finished

playGame :: AdventureGame ()
playGame = do
    finished <- playTurn
    if finished
        then do
            game <- get
            lift (putStrLn ("Final score: " ++ show (score game)))
        else playGame


getDiceRoll :: IO Int
getDiceRoll = do
    putStrLn "Enter dice roll result:"
    input <- getLine

    let dice = read input :: Int

    if dice >= 1 && dice <= 6
        then return dice
        else do
            putStrLn "Invalid dice roll. Enter a number from 1 to 6."
            getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState game = do
    putStrLn "----------------------------"
    putStrLn ("Path:     " ++ path game)
    putStrLn ("Position: " ++ show (position game))
    putStrLn ("Energy:   " ++ show (energy game))
    putStrLn ("Score:    " ++ show (score game))
    putStrLn "----------------------------"

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
    putStrLn "Choose a path:"
    printOptions options

    choice <- getLine

    if choice `elem` options
        then return choice
        else do
            putStrLn "Invalid choice."
            getPlayerChoice options

printOptions :: [String] -> IO ()
printOptions [] = return ()
printOptions (x:xs) = do
    putStrLn ("- " ++ x)
    printOptions xs


initialGameState :: GameState
initialGameState = GameState
    { position = 0
    , energy = 20
    , score = 0
    , path = "main"
    }

main :: IO ()
main = do
    putStrLn "Treasure Hunters"
    displayGameState initialGameState
    evalStateT playGame initialGameState

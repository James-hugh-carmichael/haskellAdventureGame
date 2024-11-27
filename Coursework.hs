import Data.List (delete, elemIndex, sort)
import Data.Char (isDigit)
------------------------- Game world types

type Character = String
type Party     = [Character]

type Node      = Int
type Location  = String
type Map       = [(Node,Node)]

data Game      = Over
               | Game Map Node Party [Party]
  deriving (Eq,Show)

type Event     = Game -> Game


testGame :: Node -> Game
testGame i = Game [(0,1)] i ["Russell"] [[],["Brouwer","Heyting"]]


------------------------- Assignment 1: The game world

connected :: Map -> Node -> [Node]
connected [] node = []
connected (x:xs) node
    |node == fst x || node == snd x = connection x node : connected xs node
    |otherwise = connected xs node
    where
        connection places node
            |node == fst places = snd places
            |otherwise       = fst places

connect :: Node -> Node -> Map -> Map
connect node1 node2 placeConnections
    |node1 == node2 = placeConnections
    |otherwise = compare (min node1 node2, max node1 node2) placeConnections
    where
        compare (node1, node2) connections =
            takeWhile (\(n,_) -> n < node1) connections ++
            sort (node1,node2) (takeWhile (\(n,_) -> n == node1) (dropWhile (\(n,_) -> n < node1) connections)) ++
            dropWhile (\(n,_) -> n <= node1) connections

        sort (node1, node2) [] = [(node1, node2)]
        sort (node1, node2) (x:xs)
            |(node1, node2) == x = xs
            |node2 < snd x = (node1, node2):x:xs
            |otherwise     = x: sort (node1, node2) xs

disconnect :: Node -> Node -> Map -> Map
disconnect node1 node2 (x:xs) =
    helper (min node1 node2, max node1 node2) (x:xs)
    where
        helper (node1, node2) [] = []
        helper (node1, node2) (x:xs)
            |(node1, node2) == x  = xs
            |otherwise = x : helper (node1,node2) xs

add :: Party -> Event
add _ Over = Over
add p (Game map node currentParty partys) =
    Game map node (noDup p currentParty) partys
    where
        noDup newCharacter current =  newCharacter ++ filter (`notElem` newCharacter) current

addAt :: Node -> Party -> Event
addAt _ _ Over = Over
addAt node party (Game map currentNode currentParty (x:xs)) =
    Game map currentNode currentParty (addHereAt node party (x:xs))

addHere :: Party -> Event
addHere _ Over = Over
addHere party (Game map currentNode currentParty (x:xs)) =
    Game map currentNode currentParty (addHereAt currentNode party (x:xs))

addHereAt :: Node -> Party -> [Party] -> [Party]
addHereAt _ _ [] = []
addHereAt 0 party (x:xs) = (x ++ party) : xs
addHereAt node party (x:xs) = x : addHereAt (node-1) party xs

remove :: Party -> Event
remove _ Over = Over
remove p (Game map node currentParty partys) =
    Game map node (removeHelper p currentParty) partys
    where
        removeHelper [] currentParty     = currentParty
        removeHelper (x:xs) currentParty = removeHelper xs (delete x currentParty)

removeAt :: Node -> Party -> Event
removeAt _ _ Over = Over
removeAt node party (Game map currentNode currentParty (x:xs)) =
    Game map currentNode currentParty (removeAtHere node party (x:xs))

removeHere :: Party -> Event
removeHere _ Over = Over
removeHere party (Game map currentNode currentParty (x:xs)) =
    Game map currentNode currentParty (removeAtHere currentNode party (x:xs))

removeAtHere :: Node -> Party -> [Party] -> [Party]
removeAtHere _ [] partylist  = partylist
removeAtHere _ _ []          = []
removeAtHere 0 (p:ps) (x:xs) = removeAtHere 0 ps (delete p x : xs)
removeAtHere node ps (x:xs)  = x : removeAtHere (node - 1) ps xs


------------------------- Assignment 2: Dialogues

prompt = ">>"
line0  = "There is nothing we can do."


data Dialogue = Action  String  Event
              | Branch  (Game -> Bool) Dialogue Dialogue
              | Choice  String  [( String , Dialogue )]

testDialogue :: Dialogue
testDialogue = Branch ( isAtZero )
  (Choice "Russell: Let's get our team together and head to Error." [])
  (Choice "Brouwer: How can I help you?"
    [ ("Could I get a haircut?", Choice "Brouwer: Of course." [])
    , ("Could I get a pint?",    Choice "Brouwer: Of course. Which would you like?"
      [ ("The Segmalt.",     Action "" id)
      , ("The Null Pinter.", Action "" id)]
      )
    , ("Will you join us on a dangerous adventure?", Action "Brouwer: Of course." (add ["Brouwer"] . removeHere ["Brouwer"]))
    ]
  )
 where
  isAtZero Over           = False
  isAtZero (Game _ n _ _) = n == 0


dialogue :: Game -> Dialogue -> IO Game
dialogue game (Action string event) = do
    putStrLn string
    return (event game)

dialogue game (Branch test option1 option2) = do
    if test game
        then do
            dialogue game option1
        else
            dialogue game option2

--------- make simpler by using multiple functions
dialogue game (Choice string choices) = do
    putStrLn string
    if null choices
      then do return game
      else do
        let indexed_list = zip [1..] choices
        helperShow indexed_list
        dialogueLoop game (Choice string choices)

          where
            helperShow [] = return ()
            helperShow ((number,(option, _)) : xs) = do
                putStrLn (show number ++": " ++ option)
                helperShow xs

            isValidSingleNumber :: String -> Bool
            isValidSingleNumber input =
                let trimmed = dropWhile (== ' ') input
                in all isDigit trimmed && length (words trimmed) == 1

            dialogueLoop :: Game -> Dialogue -> IO Game
            dialogueLoop game (Choice string choices) = do
              putStr (prompt ++ " ")
              input <- getValidInput
              if input == "0"  -- Check for "0" first to handle exit or dialogue termination
                then return game  -- End dialogue or game
                else do
                  let inputs = words input  -- Split input into separate tokens by spaces
                  if "0" `elem` inputs
                    then do return game  
                    else if not (all isDigit input)  
                      then do
                        putStrLn line6
                        dialogueLoop game (Choice string choices)
                      else do
                        if read (head inputs) >= 1 && read (head inputs) <= length choices
                            then do
                                putStrLn ""
                                dialogue game (snd (choices !! (read (head inputs) - 1)))
                            else do
                                putStrLn line6
                                dialogueLoop game (Choice string choices)

findDialogue :: Party -> Dialogue
findDialogue party  = findHelper party theDialogues
  where
    findHelper party [] = Action line0 id
    findHelper party ((x,y):xs) =
      if party == x
        then y
        else findHelper party xs

------------------------- Assignment 3: The game loop

line1 = "You are in "
line2 = "You can travel to:"
line3 = "With you are:"
line4 = "You can see:"
line5 = "What will you do?"

format :: (Show a, Show b) => [(a,b)] -> String
format [] = ""
format ((a,b):y) = show a ++ " " ++ show b ++ "\n" ++ format y

lineChecker :: (Show a, Show b) => Int -> [(a,b)] -> IO ()
lineChecker _ [] = return ()
lineChecker 2 p = putStr (line2 ++ "\n" ++ format p)
lineChecker 3 p = putStr (line3 ++ "\n" ++ format p)
lineChecker 4 p = putStr (line4 ++ "\n" ++ format p)

---------------- break into multiple functions
step :: Game -> IO Game
step (Game m n currentParty partys) = do
  putStrLn (line1 ++ (theDescriptions !! n))
  lineChecker 2 (zip [1..] displayedConnections)
  lineChecker 3 (zip [length displayedConnections + 1..] currentParty)
  lineChecker 4 (zip [length displayedConnections + length currentParty + 1..] (partys !! n))
  putStrLn line5
  stepLoop (Game m n currentParty partys)

  where
    stepLoop :: Game -> IO Game
    stepLoop (Game m n currentParty partys) = do
      putStr (prompt ++ " ")
      x <- getValidInput
      if x == "0"
        then do
          return Over
        else do
          let choices = (parse . tokenize) x
          if not (null choices) && head choices <= length displayedConnections
            then do
              let Just newNode = elemIndex (displayedConnections !! (head choices - 1)) theLocations
              putStrLn ""
              return (Game m newNode currentParty partys)
            else
              if all (\choice -> choice <= length allOptions) choices
                then do
                  let selectedParty = (sort . map (\choice -> snd (allOptions !! (choice - 1)))) choices
                  putStrLn ""
                  dialogue (Game m n currentParty partys) (findDialogue selectedParty)
                else do
                  putStrLn "There is nothing we can do."
                  stepLoop (Game m n currentParty partys)

    displayedConnections :: [String]
    displayedConnections = map (theLocations !!) (connected m n)

    allOptions :: [(Int, String)]
    allOptions = zip [1..] (displayedConnections ++ currentParty ++ (partys !! n))

tokenize :: String -> [String]
tokenize [] = []
tokenize s =
  let token = takeWhile (/= ' ') s
      rest = dropWhile (== ' ') (dropWhile (/= ' ') s)
  in token : tokenize rest

parse :: [String] -> [Int]
parse = map read

game :: IO ()
game = loop start
  where
    loop :: Game -> IO()
    loop gameState = do
      if gameState == Over
        then do
          return ()
        else do
          newState <- step gameState
          loop newState

------------------------- Assignment 4: Safety upgrades

line6 = "[Unrecognized input]"

getValidInput :: IO String
getValidInput = do
  let isAllowed :: Char -> Bool
      isAllowed c = isDigit c || c == ' '

  let isValidInput :: String -> Bool
      isValidInput x = not (null trimmed) && all isAllowed trimmed
        where trimmed = dropWhile (== ' ') x

  x <- getLine
  if "0" `elem` words x
    then do return "0"
    else do 
      let trimmed = dropWhile (== ' ') x
      if isValidInput trimmed
        then return trimmed
        else do
          putStrLn line6
          putStr (prompt ++ " ")
          getValidInput

------------------------- Assignment 5: Solving the game

data Command  = Travel [Int] | Select Party | Talk [Int]
  deriving Show

type Solution = [Command]

talk ::Game -> Dialogue -> [(Game,[Int])]
talk = undefined

select :: Game -> [Party]
select = undefined

travel :: Map -> Node -> [(Node,[Int])]
travel = undefined

allSteps :: Game -> [(Solution,Game)]
allSteps = undefined

solve :: Game -> Solution
solve = undefined

walkthrough :: IO ()
walkthrough = (putStrLn . unlines . filter (not . null) . map format . solve) start
  where
    format (Travel []) = ""
    format (Travel xs) = "Travel: " ++ unwords (map show xs)
    format (Select xs) = "Select: " ++ foldr1 (\x y -> x ++ ", " ++ y) xs
    format (Talk   []) = ""
    format (Talk   xs) = "Talk:   " ++ unwords (map show xs)


------------------------- Game data

start :: Game
start = Game theMap 0 [] theCharacters

theMap :: Map
theMap = [(1,2),(1,6),(2,4)]

theLocations :: [Location]
theLocations =
  -- Logicester
  [ "Home"           -- 0
  , "Brewpub"        -- 1
  , "Hotel"          -- 2
  , "Hotel room n+1" -- 3
  , "Temple"         -- 4
  , "Back of temple" -- 5
  , "Takeaway"       -- 6
  , "The I-50"       -- 7
  ]

theDescriptions :: [String]
theDescriptions =
  [ "your own home. It is very cosy."
  , "the `Non Tertium Non Datur' Brewpub & Barber's."
  , "the famous Logicester Hilbert Hotel & Resort."
  , "front of Room n+1 in the Hilbert Hotel & Resort. You knock."
  , "the Temple of Linearity, Logicester's most famous landmark, designed by Le Computier."
  , "the back yard of the temple. You see nothing but a giant pile of waste paper."
  , "Curry's Indian Takeaway, on the outskirts of Logicester."
  , "a car on the I-50 between Logicester and Computerborough. The road is blocked by a large, threatening mob."
  ]

theCharacters :: [Party]
theCharacters =
  [ ["Bertrand Russell"]                    -- 0  Home
  , ["Arend Heyting","Luitzen Brouwer"]     -- 1  Brewpub
  , ["David Hilbert"]                       -- 2  Hotel
  , ["William Howard"]                      -- 3  Hotel room n+1
  , ["Jean-Yves Girard"]                    -- 4  Temple
  , []                                      -- 5  Back of temple
  , ["Haskell Curry", "Jean-Louis Krivine"] -- 6  Curry's takeaway
  , ["Gottlob Frege"]                       -- 7  I-50
  ]

theDialogues :: [(Party,Dialogue)]
theDialogues = let
  always _ = True
  end str  = Choice str []
  isconn  _ _  Over           = False
  isconn  i j (Game m _ _ _ ) = elem i (connected m j)
  here         Over           = 0
  here        (Game _ n _ _ ) = n
  inParty   _  Over           = False
  inParty   c (Game _ _ p _ ) = elem c p
  isAt    _ _  Over           = False
  isAt    n c (Game _ _ _ ps) = elem c (ps !! n)
  updateMap _  Over           = Over
  updateMap f (Game m n p ps) = Game (f m) n p ps
 in
  [ ( ["Russell"] , Choice "Russell: Let's go on an adventure!"
      [ ("Sure." , end "You pack your bags and go with Russell.")
      , ("Maybe later.", end "Russell looks disappointed.")
      ]
    )
  , ( ["Heyting","Russell"] , end "Heyting: Hi Russell, what are you drinking?\nRussell: The strong stuff, as usual." )
  , ( ["Bertrand Russell"] , Branch (isAt 0 "Bertrand Russell") ( let
      intro = "A tall, slender, robed character approaches your home. When he gets closer, you recognise him as Bertrand Russell, an old friend you haven't seen in ages. You invite him in.\n\nRussell: I am here with a important message. The future of Excluded-Middle Earth hangs in the balance. The dark forces of the Imperator are stirring, and this time, they might not be contained.\n\nDo you recall the artefact you recovered in your quest in the forsaken land of Error? The Loop, the One Loop, the Loop of Power? It must be destroyed. I need you to bring together a team of our finest Logicians, to travel deep into Error and cast the Loop into lake Bottom. It is the only way to terminate it."
      re1   = ("What is the power of the Loop?" , Choice "Russell: for you, if you put it on, you become referentially transparent. For the Imperator, there is no end to its power. If he gets it in his possession, he will vanquish us all." [re2])
      re2   = ("Let's go!" , Action "Let's put our team together and head for Error." (updateMap (connect 1 0) . add ["Bertrand Russell"] . removeHere ["Bertrand Russell"]) )
      in Choice intro [re1,re2]
      ) ( Branch ( (==7).here) (end "Russell: Let me speak to him and Brouwer."
      ) (end "Russell: We should put our team together and head for Error." ) )
    )
  , ( ["Arend Heyting"] , Choice "Heyting: What can I get you?"
      [ ( "A pint of Ex Falso Quodbibet, please." , end "There you go." )
      , ( "The Hop Erat Demonstrandum, please."   , end "Excellent choice." )
      , ( "Could I get a Maltus Ponens?"          , end "Mind, that's a strong one." )
      ]
    )
  , ( ["Luitzen Brouwer"] , Branch (isAt 1 "Luitzen Brouwer")
      ( Choice "Brouwer: Haircut?"
        [ ( "Please." , let
          intro = "Brouwer is done and holds up the mirror. You notice that one hair is standing up straight."
          r1 i  = ( "There's just this one hair sticking up. Could you comb it flat, please?" , d i)
          r2    = ( "Thanks, it looks great." , end "Brouwer: You're welcome.")
          d  i  | i == 0    = Choice intro [r2]
                | otherwise = Choice intro [r1 (i-1),r2]
        in d 100)
        , ( "Actually, could you do a close shave?" , end "Of course. I shave everyone who doesn't shave themselves." )
        , ( "I'm really looking for help." , Choice "Brouwer: Hmmm. What with? Is it mysterious?"
          [ ( "Ooh yes, very. And dangerous." , Action "Brouwer: I'm in!" (add ["Luitzen Brouwer"] . removeHere ["Luitzen Brouwer"]) )
          ] )
        ]
      )
      ( end "Nothing" )
    )
  , ( ["David Hilbert"] , Branch (not . isconn 2 3) (let
        intro = "You wait your turn in the queue. The host, David Hilbert, puts up the first guest in Room 1, and points the way to the stairs.\n\nYou seem to hear that the next couple are also put up in Room 1. You decide you must have misheard. It is your turn next.\n\nHilbert: Lodging and breakfast? Room 1 is free."
        re1   = ("Didn't you put up the previous guests in Room 1, too?" , Choice "Hilbert: I did. But everyone will move up one room to make room for you if necessary. There is always room at the Hilbert Hotel & Resort." [("But what about the last room? Where do the guests in the last room go?" , Choice "Hilbert: There is no last room. There are always more rooms." [("How can there be infinite rooms? Is the hotel infinitely long?" , Choice "Hilbert: No, of course not! It was designed by the famous architect Zeno Hadid. Every next room is half the size of the previous." [re2])])])
        re2   =  ("Actually, I am looking for someone." , Action "Hilbert: Yes, someone is staying here. You'll find them in Room n+1. Through the doors over there, up the stairs, then left." (updateMap (connect 2 3)))
      in Choice intro [re1,re2]
      ) (end "Hilbert seems busy. You hear him muttering to himself: Problems, problems, nothing but problems. You decide he has enough on his plate and leave." )
    )
  , ( ["William Howard"] ,  Branch (isAt 3 "William Howard")
      (Choice "Howard: Yes? Are we moving up again?" [("Quick, we need your help. We need to travel to Error." , Action "Howard: Fine. My bags are packed anyway, and this room is tiny. Let's go!" (add ["William Howard"] . removeAt 3 ["William Howard"]))]
      ) (Branch (isAt 6 "William Howard") (Choice "Howard: What can I get you?"
        [ ("The Lambda Rogan Josh with the Raita Monad for starter, please." , end "Coming right up.")
        , ("The Vindaloop with NaN bread on the side." , Choice "Howard: It's quite spicy." [("I can handle it." , end "Excellent." ) ] )
        , ("The Chicken Booleani with a stack of poppadums, please.", end "Good choice." )
        ]
      ) (end "Howard: We need to find Curry. He'll know the way.")
    ) )
  , ( ["Jean-Yves Girard"] , Branch (isconn 4 5)  (end "You have seen enough here.") (Action "Raised on a large platform in the centre of the temple, Girard is preaching the Linearity Gospel. He seems in some sort of trance, so it is hard to make sense of, but you do pick up some interesting snippets. `Never Throw Anything Away' - you gather they must be environmentalists - `We Will Solve Church's Problems', `Only This Place Matters'... Perhaps, while he is speaking, now is a good time to take a peek behind the temple..." (updateMap (connect 4 5) ))
    )
  , ( ["Vending machine"] , Choice "The walls of the Temple of Linearity are lined with vending machines. Your curiosity gets the better of you, and you inspect one up close. It sells the following items:"
      [ ( "Broccoli"  , end "You don't like broccoli." )
      , ( "Mustard"   , end "It might go with the broccoli." )
      , ( "Watches"   , end "They seem to have a waterproof storage compartment. Strange." )
      , ( "Camels"    , end "You don't smoke, but if you did..." )
      , ( "Gauloises" , end "You don't smoke, but if you did..." )
      ]
    )
  , ( ["Jean-Louis Krivine"] , end "Looking through the open kitchen door, you see the chef doing the dishes. He is rinsing and stacking plates, but it's not a very quick job because he only has one stack. You also notice he never passes any plates to the front. On second thought, that makes sense - it's a takeaway, after all, and everything is packed in cardboard boxes. He seems very busy, so you decide to leave him alone."
    )
  , ( ["Haskell Curry"] , Branch (isAt 6 "Haskell Curry")
      (Choice "Curry: What can I get you?"
        [ ("The Lambda Rogan Josh with the Raita Monad for starter, please." , end "Coming right up.")
        , ("The Vindaloop with NaN bread on the side." , Choice "Curry: It's quite spicy." [("I can handle it." , end "Excellent." ) ] )
        , ("The Chicken Booleani with a stack of poppadums, please.", end "Good choice." )
        , ("Actually, I am looking for help getting to Error." , end "Curry: Hmm. I may be able to help, but I'll need to speak to William Howard.")
        ]
      ) (end "Nothing")
    )
  , ( ["Haskell Curry","William Howard"] , Branch (not . isconn 6 7) (Action "Curry:  You know the way to Error, right?\nHoward: I thought you did?\nCurry:  Not really. Do we go via Computerborough?\nHoward: Yes, I think so. Is that along the I-50?\nCurry:  Yes, third exit. Shall I go with them?\nHoward: Sure. I can watch the shop while you're away." (add ["Haskell Curry"] . removeAt 6 ["Haskell Curry"] . addAt 6 ["William Howard"] . remove ["William Howard"] . updateMap (connect 6 7) )) (end "It's easy, just take the third exit on I-50.")
    )
  , ( ["Gottlob Frege"] , end "A person who appears to be the leader of the mob approaches your vehicle. When he gets closer, you recognise him as Gottlob Frege. You start backing away, and he starts yelling at you.\n\nFrege: Give us the Loop! We can control it! We can wield its power!\n\nYou don't see a way forward. Perhaps Russell has a plan." )
  , ( ["Bertrand Russell","Gottlob Frege","Luitzen Brouwer"] , let
        intro = "Frege is getting closer, yelling at you to hand over the Loop, with the mob on his heels, slowly surrounding you. The tension in the car is mounting. But Russell calmly steps out to confront Frege.\n\nRussell:"
        re1   = ( "You cannot control its power! Even the very wise cannot see all ends!" , Choice "Frege: I can and I will! The power is mine!\n\nRussell:" [re2,re3] )
        re2   = ( "Brouwer, whom do you shave?" , Choice "Brouwer: Those who do not shave themselves. Obviously. Why?\n\nRussell:" [re3] )
        re3   = ( "Frege, answer me this: DOES BROUWER SHAVE HIMSELF?" , Action
                  "Frege opens his mouth to shout a reply. But no sound passes his lips. His eyes open wide in a look of bewilderment. Then he looks at the ground, and starts walking in circles, muttering to himself and looking anxiously at Russell. The mob is temporarily distracted by the display, uncertain what is happening to their leader, but slowly enclosing both Frege and Russell. Out of the chaos, Russell shouts:\n\nDRIVE, YOU FOOLS!\n\nYou floor it, and with screeching tires you manage to circle around the mob. You have made it across.\n\nEND OF ACT 1. To be continued..."
                  (const Over)
                )
      in Choice intro [re1,re2,re3]
    )
  , ( ["Bertrand Russell","Haskell Curry","Luitzen Brouwer"] , Branch ((==7).here) (end "Road trip! Road trip! Road trip!") (end "Let's head for Error!")
    )
  ]


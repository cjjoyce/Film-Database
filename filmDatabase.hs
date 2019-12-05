import Data.List
import Data.Char

-- Types
type FilmName = String
type Director = String
type Year = Int
type User = String
type Likers = [User]
type Dislikers = [User]
type Database = [Film]
type Rating = Int

-- Define Film type here 
type Film = (FilmName, Director, Year, Likers, Dislikers)

-- Rounds a Float to the nearest Int
toInt :: Float -> Int
toInt x = round x

-- Calculates the length of a list
listLength :: [a] -> Float
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

-- Adds a new film to the database
addFilm :: FilmName -> Director -> Year -> Database -> [Film]
addFilm filmName director year database  = database ++ [(filmName, director, year, [], [])]

-- Gives all films
getAllFilms :: [Film]
getAllFilms = testDatabase

-- Returns all films as strings
filmsAsString :: Database -> String
filmsAsString [] = ""
filmsAsString ((filmName, director, year, likes, dislikes):xs) = "\nTitle: " ++ filmName ++ "\nDirector: " ++ director  ++ "\nYear: " ++ show (year) ++ "\nLikes: " ++ show(listLength (likes))++ "\nDislikes: " ++ show(listLength (dislikes)) ++ "\n" ++ filmsAsString xs


-- Gives all films by a given director
getFilmsByDirector :: Director -> Database -> [Film]
getFilmsByDirector dir database = [(filmName, director, year, likes, dislikes)|(filmName, director, year, likes, dislikes) <- database, dir == director]

getFilmNamesByDirector :: Director -> Database -> [FilmName]
getFilmNamesByDirector dir database = [filmName|(filmName, director, year, likes, dislikes) <- database, dir == director]

-- Calculates a film's rating
getRating :: FilmName -> Database -> Rating
getRating film ((filmName, director, year, likes, dislikes):xs)
    |film == filmName = toInt (100 / (listLength likes + listLength dislikes) * listLength likes)
    |otherwise = getRating film xs


-- Gets all films that have a website rating of 75% or higher
getFilmsOfHighRating :: Database -> [Film]
getFilmsOfHighRating database = [(filmName, director, year, likes, dislikes)|(filmName, director, year, likes, dislikes) <- database, (getRating filmName database) >= 75]


getFilmByUser :: User -> Database -> [Film]
getFilmByUser usr database = [(filmName, director, year, likes, dislikes)|(filmName, director, year, likes, dislikes) <- database, elem usr likes || elem usr dislikes]


-- Allows a user to say they like a particular film
addLike :: User -> FilmName -> Database -> [Film]
addLike usr film database = [if film == filmName && elem usr likes == False then (filmName, director, year, likes++[usr], filter (/= usr) dislikes) else (filmName, director, year, likes, dislikes) | (filmName, director, year, likes, dislikes) <- database ]

-- Allows a user to say they dislike a particular film
addDislike :: User -> FilmName -> Database -> [Film]
addDislike usr film database = [if film == filmName && elem usr dislikes == False then (filmName, director, year, filter (/= usr) likes, dislikes++[usr]) else (filmName, director, year, likes, dislikes) | (filmName, director, year, likes, dislikes) <- database ]

getFilmsBetweenYears :: Year -> Year -> Database -> [Film]
getFilmsBetweenYears firstYear lastYear database = [(filmName, director, year, likes, dislikes)|(filmName, director, year, likes, dislikes) <- database, year >= firstYear && year <= lastYear]


-- Demo function to test basic functionality
-- (filmName, director, year, likes, dislikes)
testDatabase :: [Film]
testDatabase = [("Blade Runner", "Ridley Scott", 1982, ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"], ["Sam", "Olga", "Tim"]),

                ("The Fly", "David Cronenberg", 1986, ["Garry", "Dave", "Zoe"], ["Kevin", "Emma", "Heidi", "Jo", "Kate"]),

                ("Body Of Lies", "Ridley Scott", 2008, ["Garry", "Dave"], ["Bill", "Olga", "Tim", "Zoe", "Paula"]),

                ("Avatar", "James Cameron", 2009, ["Dave", "Amy", "Liz"], ["Olga", "Tim", "Zoe", "Paula"]),

                ("Titanic", "James Cameron", 1997 ,["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"], ["Sam", "Wally", "Kate"]),

                ("The Departed", "Martin Scorsese", 2006, ["Wally", "Liz", "Kevin", "Tim", "Emma"], ["Olga", "Dave", "Kate", "Zoe"]),

                ("Aliens", "Ridley Scott", 1986, ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"], ["Tim", "Emma", "Jo", "Olga"]),

                ("Kingdom Of Heaven", "Ridley Scott", 2005, ["Jo", "Wally", "Emma"], ["Tim", "Garry", "Ian", "Neal"]),

                ("Alien: Covenant", "Ridley Scott", 2017, ["Kevin", "Tim"], ["Emma", "Jo", "Liz"]),

                ("E.T. The Extra-Terrestrial", "Steven Spielberg", 1982, ["Dave", "Amy", "Garry", "Ian", "Neal"], ["Jenny", "Kate", "Emma", "Olga"]),

                ("Bridge of Spies", "Steven Spielberg", 2015, ["Wally", "Sam", "Dave", "Neal"], ["Bill", "Garry", "Ian", "Kate"]),

                ("Jaws", "Steven Spielberg", 1975, ["Jenny", "Emma", "Bill", "Neal"], ["Sam", "Ian", "Kate"]),

                ("The Martian", "Ridley Scott", 2015, ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"], ["Ian", "Neal", "Tim", "Liz"]),

                ("The BFG", "Steven Spielberg", 2016, ["Sam", "Wally", "Dave", "Jo", "Kate"], ["Neal"]),

                ("The Shawshank Redemption", "Frank Darabont", 1994, ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe", "Heidi"], ["Jo"]),

                ("Gladiator", "Ridley Scott", 2000, ["Olga", "Neal", "Kate", "Garry"], ["Heidi", "Bill", "Sam", "Zoe"]),

                ("The Green Mile", "Frank Darabont", 1999, ["Kevin", "Tim", "Emma", "Heidi"], ["Kate", "Jenny", "Zoe"]),

                ("True Lies", "James Cameron", 1994, ["Sam", "Dave"], ["Emma", "Olga", "Jenny", "Zoe"]),

                ("Super 8", "J J Abrams", 2011, ["Kevin", "Tim", "Emma", "Olga", "Heidi"], ["Wally", "Dave", "Jenny", "Zoe"]),

                ("Minority Report", "Steven Spielberg", 2002, ["Kevin", "Kate", "Tim", "Emma", "Jenny", "Zoe"], ["Olga", "Heidi"]),

                ("War Horse", "Steven Spielberg", 2011, ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"], ["Heidi", "Jenny", "Sam"]),

                ("Silence", "Martin Scorsese", 2016, ["Wally", "Emma", "Tim", "Heidi", "Bill", "Jo"], ["Dave", "Olga"]),

                ("The Terminal", "Steven Spielberg", 2004, ["Kate", "Dave", "Jo", "Wally", "Emma"], ["Heidi"]),

                ("Star Wars: The Force Awakens", "J J Abrams", 2015, ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz"], ["Olga", "Jo", "Neal"]),

                ("Hugo", "Martin Scorsese", 2011, ["Wally", "Sam"], ["Kate", "Bill", "Dave"])]


-- User Interface Code

main :: IO ()
main = do
    putStrLn("")
    putStrLn("Film Database")
    putStrLn("----------------")
    loadedFile <- readFile "films.txt"
    let database = read loadedFile
    putStrLn("Loaded "++ show(length database) ++" films.")
    putStrLn("")
    putStr("Please input your name: ")
    user <- getLine
    putStrLn ("\n")
    uiDisplayMenu database user

uiGetUsername :: [Film] -> IO ()
uiGetUsername database = do
        putStr "Enter name: "
        user <- getLine
        if user == ""
            then uiGetUsername database
        else uiDisplayMenu database user

uiGetFilms :: Database -> User -> IO()
uiGetFilms database user = do
    putStrLn("")
    putStrLn("Films within the database:")
    putStrLn( filmsAsString database )
    putStrLn("")
    uiDisplayMenu database user


uiExit :: Database -> User -> IO ()
uiExit database user = do
    length database `seq` writeFile "films.txt" (show database)
    putStrLn "All changes saved"


-- Adds a new film to the database
uiAddFilm :: [Film] -> String -> IO ()
uiAddFilm database user = do
    putStr "Enter Film name: "
    filmName <- getLine
    putStrLn ""
    putStr "Enter Director: "
    director <- getLine
    putStrLn ""
    putStr "Enter Year: "
    year <- getLine
    putStrLn ""
    let newDatabase = addFilm filmName director (read year :: Int) database
    putStrLn ""
    putStrLn "Film added"
    uiDisplayMenu newDatabase user

-- Gives all films by a given director
uiFilmsByDirector :: Database -> User -> IO ()
uiFilmsByDirector database user = do
    putStr "Enter Name of Director: "
    director <- getLine
    if director == ""
        then do
            uiFilmsByDirector database user
    else do
        putStr (filmsAsString (getFilmsByDirector director database))
        uiDisplayMenu database user


-- Allows the user to say they like a particular film
uiAddLike :: Database -> User -> IO ()
uiAddLike database user = do
    putStr "Enter Title of Film: "
    filmName <- getLine
    if filmName == ""
        then do
            uiAddLike database user
    else do
        let newDatabase = addLike user filmName database
        putStrLn("Added "++ user ++" to the like list for "++ filmName)
        putStrLn(filmsAsString newDatabase)
        putStrLn("")
        uiDisplayMenu newDatabase user

-- Allows the user to say they like a particular film
uiAddDislike :: Database -> User -> IO ()
uiAddDislike database user = do
    putStr "Enter Title of Film: "
    filmName <- getLine
    if filmName == ""
        then do
            uiAddDislike database user
    else do
        let newDatabase = addDislike user filmName database
        putStrLn("Added "++ user ++" to the dislike list for "++ filmName)
        putStrLn(filmsAsString newDatabase)
        putStrLn("")
        uiDisplayMenu newDatabase user

-- Displays all films that have a website rating of 75% or higher
uiFilmsOfHighRating :: Database -> User -> IO ()
uiFilmsOfHighRating database user = do
    putStr "Films of Rating above 75%: \n"
    putStr(filmsAsString (getFilmsOfHighRating database))
    uiDisplayMenu database user

-- Displays all films that a particular user has rated.
uiGetFilmsByUser :: Database -> User -> IO ()
uiGetFilmsByUser database user = do
    putStr "Enter name: \n"
    name <- getLine
    putStr(filmsAsString (getFilmByUser name database))
    uiDisplayMenu database user

-- Displays all films released inclusively between two given years in descending order of website rating.
uiFilmsBetweenYears :: Database -> User -> IO ()
uiFilmsBetweenYears database user = do
    putStr "First Year: "
    firstYear <- getLine
    putStrLn ""
    putStr "Second Year: "
    secondYear <- getLine
    let films = getFilmsBetweenYears (read firstYear :: Int) (read secondYear :: Int) database
    if films == []
        then do
            putStrLn "No films were found between these years."
        else 
            uiGetFilms films user
    uiDisplayMenu database user



uiDisplayMenu :: [Film] -> String -> IO ()
uiDisplayMenu database user = do
    putStrLn ""
    putStrLn "Main Menu"
    putStrLn "1 - Add a new film to the database."
    putStrLn "2 - Give all films in the database."
    putStrLn "3 - Give all films by a given director."
    putStrLn "4 - Give all films that have a website rating of 75% or higher."
    putStrLn "5 - Give the titles of the films that a particular user has rated."
    putStrLn "6 - Like a particular film."
    putStrLn "7 - Dislike a particular film."
    putStrLn "8 - Give all the films released inclusively between two given years in descending order of website rating."
    putStrLn "9 - Save and Exit."
    putStrLn ""
    putStrLn "Action : "
    action <- getLine
    putStrLn ""
    uiAction action database user

uiAction :: String -> Database -> User -> IO ()
uiAction "1" database user = uiAddFilm database user
uiAction "2" database user = uiGetFilms database user
uiAction "3" database name = uiFilmsByDirector database name
uiAction "4" database name = uiFilmsOfHighRating database name
uiAction "5" database name = uiGetFilmsByUser database name
uiAction "6" database name = uiAddLike database name
uiAction "7" database name = uiAddDislike database name
uiAction "8" database name = uiFilmsBetweenYears database name
uiAction "9" database user = uiExit database user
uiAction _ database user = uiDisplayMenu database user

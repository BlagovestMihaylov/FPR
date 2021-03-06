import           System.IO ()


--Task 1

type Name = String
type Title = String
type Year = Int
type Gender = Char
type Length = Int

data Movie = Movie {getMovieTitle :: Title, getYear :: Year, getLength :: Length} deriving Show

data MovieStar = MovieStar {getStarName :: Name, getGender :: Gender} deriving Show

data StarsIn = StarsIn {getName :: Name, getTitle :: Title} deriving Show

type MovieDB = ([Movie], [MovieStar], [StarsIn])

-- като цяло беше копи-пейст от условието

movies :: [Movie]                                
movies = [Movie "The Man Who Wasn't There" 2001 116,
 Movie "Logan's run" 1976 120,
 Movie "Star Wars" 1977 124,
 Movie "Empire Strikes Back" 1980 111,
 Movie "Star Trek" 1979 132,
 Movie "Star Trek: Nemesis" 2002 116,
 Movie "Terms of Endearment" 1983 132,
 Movie "The Usual Suspects" 1995 106,
 Movie "Gone With the Wind" 1938 238,
 Movie "The Fellowship of the Ring" 2001 178]

stars :: [MovieStar]
stars = [MovieStar "Jane Fonda" 'F',
 MovieStar "Alec Baldwin" 'M',
 MovieStar "Kim Basinger" 'F',
 MovieStar "Harrison Ford" 'M',
 MovieStar "Debra Winger" 'F',
 MovieStar "Jack Nicholson" 'M',
 MovieStar "Sandra Bullock" 'F',
 MovieStar "Orlando Bloom" 'M',
 MovieStar "Cate Blanchett" 'F',
 MovieStar "Liv Tyler" 'F',
 MovieStar "Billy Bob Thornton" 'M',
 MovieStar "Scarlett Johansson" 'F']

starsIn :: [StarsIn]
starsIn = [StarsIn "Kim Basinger" "Star Wars",
 StarsIn "Alec Baldwin" "Star Wars",
 StarsIn "Harrison Ford" "Star Wars",
 StarsIn "Harrison Ford" "Empire Strikes Back",
 StarsIn "Jack Nicholson" "The Usual Suspects",
 StarsIn "Jane Fonda" "Terms of Endearment",
 StarsIn "Jack Nicholson" "Terms of Endearment",
 StarsIn "Sandra Bulloc" "The Usual Suspects",
 StarsIn "Billy Bob Thornton" "The Man Who Wasn't There",
 StarsIn "Scarlett Johansson" "The Man Who Wasn't There",
 StarsIn "Orlando Bloom" "The Fellowship of the Ring",
 StarsIn "Cate Blanchett" "The Fellowship of the Ring",
 StarsIn "Liv Tyler" "The Fellowship of the Ring"]


db :: MovieDB
db = (movies, stars, starsIn)

getLengthByName :: Title -> [Movie] -> Length
getLengthByName _ [] = 0
getLengthByName name (x:xs)
    | name == getMovieTitle x = getLength x
    | otherwise = getLengthByName name xs

getGenderByName :: Name -> [MovieStar] -> Gender
getGenderByName _ [] = 'N'
getGenderByName name (x:xs)
    | name == getStarName x = getGender x
    | otherwise = getGenderByName name xs

checkIfActorPlay :: Title -> Year -> [Movie] -> Bool
checkIfActorPlay _ _ [] = False
checkIfActorPlay title year (x:xs)
    | title == getMovieTitle x && year == getYear x = True
    | otherwise = checkIfActorPlay title year xs


getMoviesLongerThan :: Title -> MovieDB -> [Title]
getMoviesLongerThan name (movies, _, _) = helper (getLengthByName name movies) movies
    where helper :: Length -> [Movie] -> [Title]
          helper _ [] = []
          helper length movies = [getMovieTitle x | x <- movies, getLength x > length]

getMaleActorsIn :: Title -> MovieDB -> [Name]
getMaleActorsIn name (movies, movieStar, starsIn) = [getName x | x <- starsIn, name == getTitle x, getGenderByName (getName x) movieStar == 'M']

getFemaleActorsFrom :: Year -> MovieDB -> [Name]
getFemaleActorsFrom year (movies, movieStar, starsIn) = [getName x | x <- starsIn, getGenderByName (getName x) movieStar == 'F', checkIfActorPlay (getTitle x) year movies]


-- Task2

data NTree a = Nil | Node a [NTree a] deriving Show

t1 :: NTree Int
t1 = Node 8 [Node 7 [Node 4 [Nil], Node 5 [Nil]], Node 6 [Node 10 [Nil], Node 15[Nil],Node 13[Nil]], Node 18 [Nil]]

t2 :: NTree Char
t2 = Node '1' [Node 'f' [Node 'H' [Nil], Node 'a' [Nil]], Node 'm' [Node 's' [Nil]], Node 'i' [Node 'k' [Node 'e' [Nil], Node 'T' [Nil], Node 'L' [Nil]]]]                                      --     / |   |     \
                                                                        
                                                                  
                                                                        



degr :: (Eq a) => NTree a -> a -> Int
degr Nil _ = 0
degr (Node n otherTrees) a        
    | a == n = length otherTrees                           -- проверка дали корена ни е избрания елемент ако не е тръгваме по стъблото
    | otherwise = helper otherTrees a                    

helper :: (Eq a) => [NTree a] -> a -> Int
helper [Nil] _ = 0                                      --проверка дали листото е желания елемент ако е връщаме 1, ако не е връщаме 0
helper [Node a [Nil]] n                                 
    | a == n = 1                                        
    | otherwise = 0                                     
helper((Node a otherTrees):xs) n                          -- ако ни е избрания елемен
    | a == n = length otherTrees + 1                      
    | null xs = helper otherTrees n                       -- ако има само едно дете
    | otherwise = helper otherTrees n + helper xs n       -- гледаме по другите поддървета

main :: IO ()
main = do

    

    print (getMoviesLongerThan "Star Wars" db)
    print (getMoviesLongerThan "The Fellowship of the Ring" db)
    print (getMaleActorsIn "Terms of Endearment" db)
    print (getMaleActorsIn "Star Wars" db)
    print (getFemaleActorsFrom 1983 db)
    print (getFemaleActorsFrom 2001 db)

    
    
    print(degr t1 5)  --1
    print(degr t1 6)  --4
    print(degr t1 7)  --3
    print(degr t1 18) --1
    

    print(degr t2 's') --1
    print(degr t2 'k') --4
    print(degr t2 '1') --3
    print(degr t2 'f') --3
    




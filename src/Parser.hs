import Data.List
import Data.Char
import System.IO  
import Control.Monad

data Token	 =	IF
			|	THEN
			|	ELSE
			|	NUM String
			|	ID String
			|	PLUS
			|	MINUS
			|	MULTIPLY
			|	DIVIDE
			|	GTE
			| 	GE
			|	LE
			|	LTE
			|	LPAR
			|	RPAR
			|	ARROW
			|	ASSIGN
			|	CLPAR
			|	CRPAR
			|	SRPAR
			|	SLPAR
			|	SLASH
			|	SEMICOLON
			|	COLON
			|	COMMA
			|	INVALID String
			deriving (Eq,Show)
			
			
whitespace = ['\n', '\t', ' ']
specialChar = ['=','+','/','*','<','>','[',']','(',')','{','}',';',':',',']

checkString :: [Char] -> [String]
checkString [] = []
checkString st = (removeOps st) : checkString (dropSpace (dropOps st))

splitOps :: [Char] -> String
splitOps [] = []
splitOps (x:y:xs)
	|	x == ':' && y == '='	=	[]
	|	x == '=' && y == '='	=	[]
	|	x == '>' && y == '='	=	[]
	|	x == '<' && y == '='	=	[]
	|	elem x specialChar		=	[]
	|	elem x whitespace		=	[]
	|	otherwise				=	x : splitOps (y:xs)
splitOps (x:xs) 
	|	elem x whitespace 		=	[]
	|	elem x specialChar		=	[]
	|	otherwise				=	x : splitOps (xs)

removeOps :: [Char] -> String
removeOps [] = []
removeOps (x:y:xs)
	|	x == ':' && y == '='	=	[x] ++ [y]
	|	x == '=' && y == '='	=	[x] ++ [y]
	|	x == '>' && y == '='	=	[x] ++ [y]
	|	x == '<' && y == '='	=	[x] ++ [y]
	|	elem x specialChar		=	[x]
	|	elem x whitespace		=	removeOps (y:xs)
	|	otherwise				=	x : splitOps (y:xs)
removeOps (x:xs) 
	|	elem x whitespace 		=	[]
	|	elem x specialChar		=	[x]
	|	otherwise				=	x : splitOps (xs)

dropString' :: [Char] -> String
dropString' [] = []
dropString' (x:y:xs)
	|	x == ':' && y == '='	=	(x:y:xs)
	|	x == '=' && y == '='	=	(x:y:xs)
	|	x == '>' && y == '='	=	(x:y:xs)
	|	x == '<' && y == '='	=	(x:y:xs)
	|	elem x specialChar		=	(x:y:xs)
	|	elem x whitespace 		=	(x:y:xs)
	|	otherwise				=	dropString' (y:xs)
dropString' (x:xs)
	|	elem x whitespace		=	(x:xs)
	|	elem x specialChar		=	(x:xs)
	|	otherwise				=	dropString' xs
	
dropOps :: [Char] -> String
dropOps [] = []
dropOps (x:y:xs)
	|	x == ':' && y == '=' 	=	xs
	|	x == '=' && y == '='	=	xs
	|	x == '>' && y == '='	=	xs
	|	x == '<' && y == '='	=	xs
	|	elem x specialChar		=	(y:xs)
	|	elem x whitespace 		=	(y:xs)
	|	otherwise				= 	dropString' (x:y:xs)
dropOps (x:xs)
	|	elem x whitespace		=	xs
	|	elem x specialChar		=	xs
	|	otherwise				=	dropString' (xs)

dropSpace :: [Char] -> String
dropSpace [] = []
dropSpace (x:xs)
	|	elem x whitespace 		=	dropSpace xs
	|	otherwise				=	(x:xs)

tokenize_helper :: String -> [String]
tokenize_helper st = checkString st
	
tokenizer :: String -> [Token]
tokenizer str = lexer str'
		where str' = tokenize_helper str 
			
lexer :: [String] -> [Token]
lexer [] = []
lexer ("if" : rem) = IF : lexer rem
lexer ("then" : rem) = THEN : lexer rem
lexer ("else" : rem) = ELSE : lexer rem
lexer ( op : rem )
			|	op == "+" = PLUS : lexer rem
			|	op == "*" = MULTIPLY : lexer rem
			|	op == "-" = MINUS : lexer rem
			|	op == "/" = DIVIDE : lexer rem
			|	op == ">" = GE : lexer rem
			|	op == ">=" = GTE : lexer rem
			|	op == ":=" = ASSIGN : lexer rem
			|	op == "<" = LE : lexer rem
			|	op == "<=" = LTE : lexer rem
			|	op == "(" = LPAR : lexer rem
			|	op == ")" = RPAR : lexer rem
			|	op == "{" = CLPAR : lexer rem
			|	op == "}" = CRPAR : lexer rem
			|	op == "[" = SLPAR : lexer rem
			|	op == "]" = SRPAR : lexer rem
lexer ("|" : rem) = SLASH : lexer rem	
lexer (";" : rem) = SEMICOLON : lexer rem
lexer (":" : rem) = COLON : lexer rem
lexer ("," : rem) = COMMA : lexer rem
lexer (num : rem) 
        | isDigit' first && isNumber' num = (NUM num ) : lexer rem
        | isDigit' first && not (isNumber' num) =  (INVALID (num ++ " is not a valid number" )) : lexer rem
        where first = num !! 0
lexer (iden: rem)
       | isChar first && isAlphaNum' xs = ID iden : lexer rem
       | isChar first && not (isAlphaNum' iden) =  (INVALID (iden ++ " is not a valid identifier" )) : lexer rem   
        where first = iden !! 0
              x:xs   =  iden
lexer ("/*" : rest)
             | bool == True = lexer str
             | otherwise    = [INVALID ("Lexer Error : Comment not properly closed")]
             where res = find_comment_marker rest
                   bool = fst res
                   str  = snd res

-- anything not a part of the syntax is defined as Invalid Token            
lexer (x : rem) = (INVALID ("Invalid Token"  ++ show x)) : lexer rem  

                
-- helper function to find a matching */ for /*
find_comment_marker :: [String] -> (Bool , [String])
find_comment_marker ([]) =   (False , [])
find_comment_marker (s:str) 
          | s /= "*/"  = find_comment_marker str
          | s == "*/"  = (True ,str)
          


-- checks whether a  given input character is an alphabet
isChar :: Char -> Bool
isChar x = x `elem` ['a','b'..'z']
-- checks whether a  given input character represents a number 
isDigit' :: Char -> Bool
isDigit' x = x `elem` "0123456789"

-- checks whether every  given input character in the input String represents 
-- an alphanumeric character
isAlphaNum' :: String -> Bool
isAlphaNum' str
    | False `elem` each_char = False
    | otherwise             = True
   where each_char = map (\x -> isAlphaNum x) str
             
-- checks whether every  given input character in the input String represents 
-- a digit
isNumber' :: String -> Bool
isNumber' str 
    | False `elem` each_num = False
    | otherwise             = True
   where each_num = map (\x -> isDigit' x) str

toprint []     = []             
toprint (x:xs) = (show x) ++ "\n" ++ toprint xs
				   
main = do
	putStrLn "Enter the string you want to tokenize"
	fileName <- getLine
	contents <- readFile fileName
	let token = (toprint.tokenizer) contents
	putStrLn token
	
	

	
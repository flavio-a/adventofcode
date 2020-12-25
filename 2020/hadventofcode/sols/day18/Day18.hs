import ReadFile
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data AstOp = Plus | Times
    deriving (Eq, Show)
data Expr = Const Integer | BinOp AstOp Expr Expr
    deriving (Show)

languageDef = emptyDef {
    Token.reservedOpNames = ["+", "*"]
}

lexer = Token.makeTokenParser languageDef

reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer

ops1 = [ [ Infix (reservedOp "+" >> return (BinOp Plus )) AssocLeft,
           Infix (reservedOp "*" >> return (BinOp Times)) AssocLeft]
       ]

ops2 = [ [ Infix (reservedOp "+" >> return (BinOp Plus )) AssocLeft],
         [ Infix (reservedOp "*" >> return (BinOp Times)) AssocLeft]
       ]

exprParser1 :: Parser Expr
exprParser1 = buildExpressionParser ops1 term
    where
        term = parens exprParser1
            <|> liftM Const integer

exprParser2 :: Parser Expr
exprParser2 = buildExpressionParser ops2 term
    where
        term = parens exprParser2
            <|> liftM Const integer


parseString :: Parser Expr -> String -> Expr
parseString parser str = case parse parser "" str of
    Left e  -> error $ show e
    Right r -> r

eval :: Expr -> Integer
eval (Const n) = n
eval (BinOp op rchild lchild) | op == Plus  = eval rchild + eval lchild
                              | op == Times = eval rchild * eval lchild

getSum :: Parser Expr -> String -> Integer
getSum parser = sum . map (eval . parseString parser) . lines

main = do
    content <- ReadFile.readFileArg
    -- Part 1
    putStrLn $ show $ getSum exprParser1 content
    -- Part 2
    putStrLn $ show $ getSum exprParser2 content

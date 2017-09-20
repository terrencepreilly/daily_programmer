
    import Data.Char
    import qualified Data.Set as Set
    import qualified Data.Map as Map

    ---------------------- Tokens ----------------------------
    data TokenType = Digit | Operator | Variable | Equals
        deriving (Show, Eq)

    data Token = Token { value :: String
                       , tokenType :: TokenType
                       } deriving Show

    isDigitToken = (==Digit) . tokenType
    isVariableToken = (==Variable) . tokenType
    isOperatorToken = (==Operator) . tokenType
    isEqualsToken = (==Equals) . tokenType


    ---------------------- Lexer -----------------------------
    lexDigit :: String -> (Token, String)
    lexDigit x = (token, rest)
        where
            token = Token { value = takeWhile isDigit x
                          , tokenType = Digit
                          }
            rest = dropWhile isSeparator . dropWhile isDigit $ x

    operators :: Set.Set Char
    operators = Set.fromList "+-*/"

    isOperator :: Char -> Bool
    isOperator x = Set.member x operators

    lexOperator :: String -> (Token, String)
    lexOperator x = (token, rest)
        where
            token = Token { value = takeWhile isOperator x
                          , tokenType = Operator
                          }
            rest = dropWhile isSeparator . dropWhile isOperator $ x

    lexVariable :: String -> (Token, String)
    lexVariable x = (token, rest)
        where
            token = Token { value = takeWhile isLetter x
                          , tokenType = Variable
                          }
            rest = dropWhile isSeparator . dropWhile isLetter $ x

    isEquals :: Char -> Bool
    isEquals = (=='=')

    lexEquals :: String -> (Token, String)
    lexEquals x = (token, rest)
        where
            token = Token { value = [head x]
                          , tokenType = Equals
                          }
            rest = dropWhile isSeparator . dropWhile isEquals $ x


    nextToken :: String -> (Token, String)
    nextToken x
        | isDigit (head x) = lexDigit x
        | isOperator (head x) = lexOperator x
        | isLetter (head x) = lexVariable x
        | isEquals (head x) = lexEquals x
        | otherwise = error ("Unrecognized symbol " ++ [head x])

    calclex :: String -> [Token]
    calclex "" = []
    calclex x = let token = nextToken x
            in (fst token) : calclex (snd token)

    ---------------------- Parser ----------------------------

    -- statement ::= <variable> = <expression>
    -- value ::= <digit>|<variable>
    -- expression ::= <value> [<operator> <value>]*

    data Node = Node Token [Node] deriving Show

    getToken :: Node -> Token
    getToken (Node t _) = t

    getChildren :: Node -> [Node]
    getChildren (Node _ children) = children

    addChild :: Node -> Node -> Node
    addChild parent child = Node (getToken parent) (child : getChildren parent)

    _validValue :: Token -> Bool
    _validValue x = isDigitToken x || isVariableToken x

    _parseTerminal :: (Token -> Bool) -> [Token] -> (Node, [Token])
    _parseTerminal isValid (x:xs) =
        if isValid x then (Node x [], xs)
        else error "Invalid token for terminal."

    parseDigit = _parseTerminal isDigitToken
    parseOperator = _parseTerminal isOperatorToken
    parseEquals = _parseTerminal isEqualsToken
    parseVariable = _parseTerminal isVariableToken

    parseValue :: [Token] -> (Node, [Token])
    parseValue (x:xs) = if _validValue x
                        then (Node x [], xs)
                        else error "Invalid token type for value"

    parseExpression :: [Token] -> (Node, [Token])
    parseExpression [x] = parseValue [x]
    parseExpression tokens = ((addChild (addChild op v1) expr), rest)
        where
            (v1, opExpr) = parseValue tokens
            (op, v2Expr) = parseOperator opExpr
            (expr, rest) = parseExpression v2Expr

    parseStatement :: [Token] -> (Node, [Token])
    parseStatement tokens = (addChild (addChild equals lhs) rhs, rest)
        where
            (lhs, tokens1) = parseVariable tokens
            (equals, tokens2) = parseEquals tokens1
            (rhs, rest) = parseExpression tokens2

    _isStatement :: [Token] -> Bool
    _isStatement tokens = tokenType (tokens !! 1) == Equals

    parse :: [Token] -> (Node, [Token])
    parse tokens
        | length tokens == 1 = parseExpression tokens
        | otherwise = if _isStatement tokens
                      then parseStatement tokens
                      else parseExpression tokens


    ---------------------- Interpreter -----------------------

    getLiteralValue :: Node -> Float
    getLiteralValue n = (read . value . getToken $ n) :: Float

    type State = Map.Map String Float

    evaluateLiteral :: State -> Node -> (Float, State)
    evaluateLiteral state node = (getLiteralValue node, state)

    evaluateVariable :: State -> Node -> (Float, State)
    evaluateVariable state node = (val, state)
        where
            val = case Map.lookup varName state of Nothing -> error msg
                                                   Just x -> x
            msg = "Variable " ++ varName ++ " not defined."
            varName = value $ getToken node

    _opEvaluation :: (Float -> Float -> Float) -> Float -> State -> Node -> (Float, State)
    _opEvaluation op init state node = (foldr op init childValues, state)
        where
            childValues = reverse . map fst $ map fn (getChildren node)
            fn = evaluateExpression state

    evaluateAddition = _opEvaluation (+) 0
    evaluateSubtraction = _opEvaluation (-) 0
    evaluateMultiplication = _opEvaluation (*) 1
    evaluateDivision = _opEvaluation (/) 1

    _evaluateOperator :: State -> Node -> (Float, State)
    _evaluateOperator state node
        | op == "+" = evaluateAddition state node
        | op == "-" = evaluateSubtraction state node
        | op == "*" = evaluateMultiplication state node
        | op == "/" = evaluateDivision state node
        where
            op = value . getToken $ node

    evaluateExpression :: State -> Node -> (Float, State)
    evaluateExpression state node
        | isDigitToken $ getToken node = evaluateLiteral state node
        | isVariableToken $ getToken node = evaluateVariable state node
        | isOperatorToken $ getToken node = _evaluateOperator state node

    evaluateStatement :: State -> Node -> (Float, State)
    evaluateStatement state node = (val, newState)
        where
            newState = Map.insert variableName val state
            variableName = value . getToken . last . getChildren $ node
            val = fst $ evaluateExpression state (head . getChildren $ node)

    evaluate :: State -> String -> (Float, State)
    evaluate state s = if isStatement
                       then evaluateStatement state node
                       else evaluateExpression state node
        where
            isStatement = isEqualsToken . getToken $ node
            node = fst . parse . calclex $ s

    evaluateAll :: State -> [String] -> (Float, State)
    evaluateAll state (x:xs)
        | length xs == 0 = evaluate state x
        | otherwise = evaluateAll newState xs
        where
            (_, newState) = evaluate state x

    main = interact (show . fst . (evaluateAll Map.empty) . lines)

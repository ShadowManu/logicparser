{
module Lexer (lexer) where
import Types (Token(..))
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  
  -- Literal Values
  true          { \s -> TRUE }
  false         { \s -> FALSE }

  -- Literal Symbols
  \(            { \s -> LPAREN }
  \)            { \s -> RPAREN }

  -- Binary Operators
  ===           { \s -> EQUAL }
  !==           { \s -> NOTEQUAL }
  \/\\          { \s -> AND }
  \\\/          { \s -> OR }
  =>            { \s -> IMPLIES }
  
  -- Unary Operators
  !             { \s -> NOT }

  -- Variables
  $alpha+       { \s -> VAR s }

{

lexer :: String -> [Token]
lexer = alexScanTokens

}
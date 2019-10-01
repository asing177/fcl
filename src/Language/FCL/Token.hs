{-|

Tokens for FCL lexer.

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.FCL.Token where

import Data.Text (Text)

default (Text)

global   = "global"
local    = "local"
asset    = "asset"
account  = "account"
contract = "contract"
true     = "true"
false    = "false"
void     = "void"
int      = "int"
num      = "num"
decimal  = "decimal"
bool     = "bool"
sig      = "sig"
text     = "text"
datetime = "datetime"
timedelta = "timedelta"
type_    = "type"
enum     = "enum"
map      = "map"
set      = "set"
if_      = "if"
else_    = "else"
before   = "before"
after    = "after"
between  = "between"
case_    = "case"
hole = "?"
initial    = "initial"
terminal   = "terminal"
transition = "transition"
role       = "role"
roles      = "roles"

assign   = "="
mult     = "*"
add      = "+"
sub      = "-"
div      = "/"
pow      = "^"
and      = "&&"
or       = "||"
equal    = "=="
nequal   = "!="
gequal   = ">="
lequal   = "<="
greater  = ">"
lesser   = "<"
not      = "!"
dot      = "."

semi     = ";"
colon    = ":"
at       = "@"

lparen   = "("
rparen   = ")"
lbrace   = "{"
rbrace   = "}"
lbracket = "["
rbracket = "]"

rarrow   = "->"


keywords =
  [ global
  , local
  , asset
  , account
  , contract
  , true
  , false
  , void
  , int
  , num
  , decimal
  , bool
  , sig
  , text
  , datetime
  , timedelta
  , type_
  , enum
  , map
  , set
  , if_
  , else_
  , before
  , after
  , between
  , case_
  , initial
  , terminal
  , transition
  , role
  , roles
  ]

operators = [
    mult
  , add
  , sub
  , div
  , pow
  , and
  , or
  , equal
  , nequal
  , gequal
  , lequal
  , greater
  , lesser
  , not
  , dot
  , hole
  ]

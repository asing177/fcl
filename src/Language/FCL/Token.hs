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
ref      = "ref"
sig      = "sig"
text     = "text"
datetime = "datetime"
timedelta = "timedelta"
any      = "any"
state    = "state"
type_    = "type"
map      = "map"
set      = "set"

if_      = "if"
else_    = "else"
before   = "before"
after    = "after"
between  = "between"
case_    = "case"
hole = "?"

assign   = "="
mult     = "*"
add      = "+"
sub      = "-"
div      = "/"
and      = "&&"
or       = "||"
equal    = "=="
nequal   = "!="
gequal   = ">="
lequal   = "<="
greater  = ">"
lesser   = "<"
not      = "!"

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

initial    = "initial"
terminal   = "terminal"
transition = "transition"
role       = "role"
roles      = "roles"

intBinOps  = [mult,add,sub,div]
boolBinOps = [and, or]
boolUnops  = [not]

keywords = [
    global
  , local
  , asset
  , contract
  , account
  , sig
  , text
  , true
  , false
  , int
  , num
  , decimal
  , bool
  , void
  , datetime
  , timedelta
  , if_
  , else_
  , after
  , before
  , between
  , initial
  , terminal
  , transition
  , role
  , roles
  , type_
  , case_
  , hole
  ]

operators = [
    mult
  , add
  , sub
  , div
  , and
  , or
  , equal
  , nequal
  , gequal
  , lequal
  , greater
  , lesser
  , not
  ]

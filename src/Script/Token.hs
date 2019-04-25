{-|

Tokens for FCL lexer.

--}

{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}

module Script.Token where

import Data.Text (Text)

default (Text)

global   = "global"
local    = "local"
asset    = "asset"
assetBin = "assetBin"
assetDis = "assetDisc"
assetFrac1 = "assetFrac1"
assetFrac2 = "assetFrac2"
assetFrac3 = "assetFrac3"
assetFrac4 = "assetFrac4"
assetFrac5 = "assetFrac5"
assetFrac6 = "assetFrac6"
account  = "account"
contract = "contract"
true     = "True"
false    = "False"
void     = "void"
int      = "int"
float    = "float"
fixed1   = "fixed1"
fixed2   = "fixed2"
fixed3   = "fixed3"
fixed4   = "fixed4"
fixed5   = "fixed5"
fixed6   = "fixed6"
bool     = "bool"
ref      = "ref"
sig      = "sig"
text     = "text"
datetime = "datetime"
timedelta = "timedelta"
any      = "any"
state    = "state"
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
  , assetBin
  , assetDis
  , assetFrac1
  , assetFrac2
  , assetFrac3
  , assetFrac4
  , assetFrac5
  , assetFrac6
  , contract
  , account
  , sig
  , text
  , true
  , false
  , float
  , int
  , fixed1
  , fixed2
  , fixed3
  , fixed4
  , fixed5
  , fixed6
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
  , enum
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

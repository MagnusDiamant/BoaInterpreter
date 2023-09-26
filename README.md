# BoaInterpreter
## The Boa Language

The language Boa is a tiny subset of Python 3, with the intent that valid Boa programs should normally give the same result as when run in Python.

Boa works with a type of structured values, given by the following algebraic datatype:

```haskell
data Value =
    NoneVal
    | TrueVal | FalseVal
    | IntVal Int
    | StringVal String
    | ListVal [Value]

A Boa value is one of the three special atoms None, True, or False, an integer, a character string, or a (possibly empty) list of values.

A Boa expression has one of the following forms:

```haskell
data Exp =
    Const Value
    | Var VName
    | Oper Op Exp Exp
    | Not Exp
    | Call FName [Exp]
    | List [Exp]
    | Compr Exp [CClause]

type VName = String
type FName = String

data Op = Plus | Minus | Times | Div | Mod | Eq | Less | Greater | In

data CClause =
    CCFor VName Exp
    | CCIf Exp

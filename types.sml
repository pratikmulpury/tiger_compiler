structure Types =
struct

  type unique = unit ref

  datatype ty = INT
              | STRING
              | RECORD of (Symbol.symbol * ty) list * unique
              | ARRAY of ty*unique
              | NIL
              | BOTTOM
              | UNIT
              | NAME of Symbol.symbol * ty option ref

datatype ordering = SUPERTYPE
| EQ
| SUBTYPE
| INCOMPATIBLE

  (*String representation for error messages*)
  fun toString ty = case ty of INT => "INT"
    |  STRING => "STRING"
    |  RECORD(_) => "RECORD" 
    |  ARRAY(_) => "ARRAY"
    |  NIL => "NIL"
    |  UNIT => "UNIT"
    |  NAME(_) => "NAME"
    |  BOTTOM => "ERROR"

  (*Returns true if first arg is subtype of second*)
  fun isSubtype(BOTTOM,_) = true
  | isSubtype(INT,INT) = true
  | isSubtype(STRING,STRING) = true
  | isSubtype(RECORD(_,uid1),RECORD(_,uid2)) = uid1 = uid2
  | isSubtype(ARRAY(_,uid1),ARRAY(_,uid2)) =  uid1 = uid2
  | isSubtype(NIL,RECORD _) =  true
  | isSubtype(NIL,NIL) = true
  | isSubtype(UNIT,UNIT) = true
  | isSubtype(_,_) = false

  fun compare(t1, t2) = 
    if isSubtype(t1,t2) andalso isSubtype(t2,t1)
    then EQ
    else if isSubtype(t1,t2)
      then SUBTYPE
      else 
        if isSubtype(t2,t1)
        then SUPERTYPE
        else INCOMPATIBLE
end


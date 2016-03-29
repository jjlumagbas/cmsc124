

import s-exp as S

# The structure that Pyret creates to represent an
# s-expression is defined in the s-exp module imported above as:
#data S-Exp:
#  | s-list(exps :: List<S-Exp>)
#  | s-num(n :: Number)
#  | s-str(s :: String)
#  | s-sym(s :: String)
#end

data Expr:
  | e-num(value :: Number)
  | e-op(op :: Operator, left :: Expr, right :: Expr)
end

data Operator:
  | op-plus
end

fun parse(s :: S.S-Exp) -> Expr:
  cases (S.S-Exp) s:
    | s-list(exps) =>
      cases (List<S.S-Exp>) exps:
        | empty => raise("Not a valid statement in Paret")
        | link(first, rest) =>
          cases (S.S-Exp) first:
            | s-sym(op) =>
              if op == "+":
                if rest.length() <> 2:
                  raise("+ needs 2 operands")
                else:
                  e-op(op-plus, parse(rest.first), parse(rest.last()))
                end
                
              else:
                raise(first + " not a valid operator")
              end
            | else => raise("Should start with a valid operator")
          end
      end
    | s-num(n) => e-num(n)
    | s-str(shadow s) => raise("Strings not accepted in Paret. Sorry!")
    | s-sym(shadow s) => raise("Not a valid statement in Paret")
  end
end


check:
  parse(S.s-num(3)) is e-num(3)
  parse(S.s-list([list: S.s-sym("+"), S.s-num(1), S.s-num(2)]))
    is e-op(op-plus, e-num(1), e-num(2))
end



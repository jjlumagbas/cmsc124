

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
  | op-mul
end

data ExprExt:
  | e-numExt(value :: Number)
  | e-opExt(op :: OperatorExt, left :: ExprExt, right :: ExprExt)
end


data OperatorExt:
  | op-plusExt
  | op-mulExt
  | op-minusExt
end

fun lookup-op(op :: String) -> OperatorExt:
  ask:
    | op == "+" then: op-plusExt
    | op == "*" then: op-mulExt
    | op == "-" then: op-minusExt
    | otherwise: raise("Not a valid expression")
  end
end

fun parse-binop(op :: OperatorExt, args :: List<S.S-Exp>) -> ExprExt%(is-e-opExt):
  if args.length() == 2:
    e-opExt(op, parse(args.first), parse(args.last()))
  else:
    raise("Not a valid expression")
  end
end


fun parse(s :: S.S-Exp) -> ExprExt:
  cases (S.S-Exp) s:
    | s-num(n) => e-numExt(n)
    | s-list(exps) =>
      cases (List<S.S-Exp>) exps:
        | link(first, rest) =>
          cases (S.S-Exp) first:
            | s-sym(op) =>
              parse-binop(lookup-op(op), rest)
            | else => raise("Not a valid expression")
          end
        | empty => raise("Not a valid expression")
      end
    | else => raise("Not a valid expression")
  end
end


fun desugar(e :: ExprExt) -> Expr:
  cases (ExprExt) e:
    | e-numExt(value :: Number) => e-num(value)
    | e-opExt(op :: OperatorExt, left :: ExprExt, right :: ExprExt) =>
      cases (OperatorExt) op:
        | op-plusExt => e-op(op-plus, desugar(left), desugar(right))
        | op-mulExt => e-op(op-mul, desugar(left), desugar(right)) 
        | op-minusExt => 
          e-op(op-plus, desugar(left), e-op(op-mul, e-num(-1), desugar(right)))
      end
  end
end


data Value:
  | numV(n :: Number)
  | boolV(n :: Boolean)
end

fun interp(e :: Expr) -> Value:
  cases (Expr) e:
    | e-num(value) => value
    | e-op(op, left, right) => 
      cases (Operator) op:
        | op-plus => numV(interp(left).n + interp(right).n)
        | op-mul => interp(left) * interp(right)
      end
  end
end


fun p(s): desugar(parse(S.read-s-exp(s))) end

fun eval(s): interp(p(s)) end

check:
  
  interp(p("3")) is 3
  interp(p("(+ 3 4)")) is 7
  interp(p("(* 3 4)")) is 12
  
  eval("(- 3 4)") is -1
  
 
end




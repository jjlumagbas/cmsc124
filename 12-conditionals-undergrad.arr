

import s-exp as S
import lists as L


data ExprC:
  | trueC
  | falseC
  | numC(n :: Number)
  | plusC(l :: ExprC, r :: ExprC)
  | multC(l :: ExprC, r :: ExprC)
  | numEqC(l :: ExprC, r :: ExprC)
  | ifC(c :: ExprC, t :: ExprC, e :: ExprC)
end

data ExprExt:
  | trueExt
  | falseExt
  | numExt(n :: Number)
  | plusExt(l :: ExprExt, r :: ExprExt)
  | multExt(l :: ExprExt, r :: ExprExt)
  | subExt(l :: ExprExt, r :: ExprExt)
  | numEqExt(l :: ExprExt, r :: ExprExt)
  | ifExt(c :: ExprExt, t :: ExprExt, e :: ExprExt)
end

data Value:
  | numV(n :: Number)
  | boolV(b :: Boolean)
end

# Here is a parse function that will convert an s-expression
# to this ArithExt structure, raising an exception if the 
# expression is not a valid arithmetic expression.
fun parse(sexp :: S.S-Exp) -> ExprExt:
  cases (S.S-Exp) sexp:
    | s-num(n) => numExt(n)
    | s-sym(s) =>
      if s == "true":
        trueExt
      else if s == "false":
        falseExt
      else:
        raise("parse: unexpected symbol " + s)
      end
    | s-list(s) =>
      cases (List) s:
        | empty => raise("parse: unexpected empty list")
        | link(op, rest) =>         
          argL = L.index(rest, 0)
          argR = L.index(rest, 1)
          if op.s == "+":
            plusExt(parse(argL), parse(argR))
          else if op.s == "*":
            multExt(parse(argL), parse(argR))
          else if op.s == "-":
            subExt(parse(argL), parse(argR))
          else if op.s == "if":
            ifExt(parse(argL), parse(argR), parse(L.index(rest, 2)))
          end
      end
    | else =>  
      raise("parse: not number or list")
  end
end

fun parseString(s): parse(S.read-s-exp(s)) end

check "parse":
  # Helper function to go from strings to ArithExt
  parseString("3") is numExt(3)
  parseString("(+ 1 2)") is plusExt(numExt(1), numExt(2))
  parseString("(* (+ 1 2) (* 2 5))") is
  multExt(plusExt(numExt(1), numExt(2)), multExt(numExt(2), numExt(5)))
  parseString("(- 1 2)") is subExt(numExt(1), numExt(2))
  parseString("(+ + +)") raises "parse"
  
  parseString("true") is trueExt
end

################
#
# Here is our interpreter.  This takes something that is an 
# ArithC value and evaluates it to produce a Number.


fun arith-binop(op :: (Number, Number -> Number),
    l :: ExprC,
    r :: ExprC) -> Value:
  l-v = interp(l)
  r-v = interp(r)
  if is-numV(l-v) and is-numV(r-v):
    numV(op(l-v.n, r-v.n))
  else:
    raise('argument not a number')
  end
end

fun interp(e :: ExprC) -> Value:
  cases (ExprC) e:
    | numC(n) => numV(n)
    | plusC(l, r) => arith-binop(lam(x, y): x + y end, l, r)
    | multC(l, r) => arith-binop(lam(x, y): x * y end, l, r)
    | trueC => boolV(true)
    | falseC => boolV(false)
    | ifC(cnd, thn, els) =>
      ic = interp(cnd)
      if is-boolV(ic):
        if ic.b:
          interp(thn)
        else:
          interp(els)
        end
      else:
        raise('not a boolean')
      end
  end
end



fun desugar(e :: ExprExt) -> ExprC:
  cases(ExprExt) e:
    | numExt(n :: Number) => numC(n)
    | trueExt => trueC
    | falseExt => falseC
    | plusExt(l :: ExprExt, r :: ExprExt) => plusC(desugar(l), desugar(r))
    | multExt(l :: ExprExt, r :: ExprExt) => multC(desugar(l), desugar(r))
    | subExt(l :: ExprExt, r :: ExprExt) => plusC( desugar(l), multC(numC(-1), desugar(r)))
    | ifExt(cond, thenExp, elseExp) => ifC(desugar(cond), desugar(thenExp), desugar(elseExp))
  end
end


fun interpString(expr :: String) -> Value:
  interp(desugar(parse(S.read-s-exp(expr))))
end

check "interp":
  interpString("3") is numV(3)
  interpString("(+ 1 2)") is numV(3)
  interpString("(* 1 2)") is numV(2)
  interpString("(* 1 (+ 1 2))") is numV(3)
  
  interpString("()") raises "parse"
  
  interpString("(if true 1 (+ 1 1))") is numV(1)
  interpString("(if false 1 (+ 1 1))") is numV(2)
  #interpString("(if (num= 1 2) 1 (+ 1 1))") is numV(2)
end

















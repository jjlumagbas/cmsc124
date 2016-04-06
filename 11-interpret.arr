
import s-exp as S
import lists as L

data ArithC:
  | numC(n :: Number)
  | plusC(l :: ArithC, r :: ArithC)
  | multC(l :: ArithC, r :: ArithC)
end

# Here is a parse function that will convert an s-expression
# to this ArithExt structure, raising an exception if the 
# expression is not a valid arithmetic expression.
fun parse(sexp :: S.S-Exp) -> ArithExt:
  cases (S.S-Exp) sexp:
    | s-num(n) => numExt(n)  # Numbers become numC
    | s-list(s) =>
      cases (List) s:
        | empty => raise("parse: unexpected empty list")
        | link(op, args) =>         # Lists should either be +, * or -
          argL = L.index(args, 0)   # with 2 arguments.
          argR = L.index(args, 1)
          if op.s == "+":
            plusExt(parse(argL), parse(argR))
          else if op.s == "*":
            multExt(parse(argL), parse(argR))
          else if op.s == "-":
            subExt(parse(argL), parse(argR))
          end
      end
    | else =>  
      # Anything that is not a simple arithmetic expression
      # will give an error.
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
  parseString("(+ + +)") raises "parse: not number or list"
end

################
#
# Here is our interpreter.  This takes something that is an 
# ArithC value and evaluates it to produce a Number.

fun interp(e :: ArithC) -> Number :
  cases(ArithC) e:
    | numC(n :: Number) => n  # Simple
    | plusC(l :: ArithC, r :: ArithC) => interp(l) + interp(r)
    | multC(l :: ArithC, r :: ArithC) => interp(l) * interp(r)
  end
end

data ArithExt:
  | numExt(n :: Number)
  | plusExt(l :: ArithExt, r :: ArithExt)
  | multExt(l :: ArithExt, r :: ArithExt)
  | subExt(l :: ArithExt, r :: ArithExt)
end


fun desugar(e :: ArithExt) -> ArithC:
  cases(ArithExt) e:
    | numExt(n :: Number) => numC(n)
    | plusExt(l :: ArithExt, r :: ArithExt) => plusC(desugar(l), desugar(r))
    | multExt(l :: ArithExt, r :: ArithExt) => multC(desugar(l), desugar(r))
    | subExt(l :: ArithExt, r :: ArithExt) => plusC( desugar(l), multC(numC(-1), desugar(r)))
  end
end


fun interpString(expr :: String) -> Number:
  interp(desugar(parse(S.read-s-exp(expr))))
end

check "interp":
  interpString("3") is 3
  interpString("(+ 1 2)") is 3
  interpString("(* 1 2)") is 2
  interpString("(* 1 (+ 1 2))") is 3
  
  interpString("()") raises "parse"
end
















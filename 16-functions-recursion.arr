

import s-exp as S
import lists as L


data Binding:
  | bind(name :: String, value :: Value)
end

type Environment = List<Binding>
mt-env = empty
xtnd-env = link



fun double(x): x + x end

fun quad(x): double(double(x)) end

fun const5(_): 5 end



data ExprC:
  | trueC
  | falseC
  | numC(n :: Number)
  | plusC(l :: ExprC, r :: ExprC)
  | multC(l :: ExprC, r :: ExprC)
  | numEqC(l :: ExprC, r :: ExprC)
  | ifC(c :: ExprC, t :: ExprC, e :: ExprC)
  | idC(s :: String)
  | fdC(arg :: String, body :: ExprC)
  | appC(f :: ExprC, a :: ExprC)
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
  | idExt(s :: String)
  | appExt(name :: String, arg :: ExprExt)
end

 data Value:
      | numV(n :: Number)
      | boolV(b :: Boolean)
      | closV(f :: ExprC%(is-fdC), e :: Environment)
    end

fun is-op(op :: String) -> Boolean:
  string-contains("+-*", op)
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
      else if not(is-op(s)):
        idExt(s)
      else:
        raise("parse: identifier not allowed")
      end
    | s-list(s) =>
      cases (List) s:
        | empty => raise("parse: unexpected empty list")
        | link(op, rest) =>         
          argL = L.index(rest, 0)
          if is-op(op.s) or (op.s == "if"):

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

          else:
            appExt(op.s, parse(argL))
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
  parseString("(+ x x)") is plusExt(idExt("x"), idExt("x"))
  
  parseString("(double 1)") is appExt("double", numExt(1))
end

################
#
# Here is our interpreter.  This takes something that is an 
# ArithC value and evaluates it to produce a Number.



fun arith-binop(op :: (Number, Number -> Number),
    l :: ExprC,
    r :: ExprC, 
    nv) -> Value:
  l-v = interp(l, nv)
  r-v = interp(r, nv)
  if is-numV(l-v) and is-numV(r-v):
    numV(op(l-v.n, r-v.n))
  else:
    raise('argument not a number')
  end
end

#| Substitution based

fun interp(e :: ExprC, fds :: List<FunDefC>) -> Value:
  cases (ExprC) e:
    | numC(n) => numV(n)
    | plusC(l, r) => arith-binop(lam(x, y): x + y end, l, r, fds)
    | multC(l, r) => arith-binop(lam(x, y): x * y end, l, r, fds)
    | trueC => boolV(true)
    | falseC => boolV(false)
    | ifC(cnd, thn, els) =>
      ic = interp(cnd, fds)
      if is-boolV(ic):
        if ic.b:
          interp(thn, fds)
        else:
          interp(els, fds)
        end
      else:
        raise('not a boolean')
      end
    | appC(name, arg) =>
      fd = get-fundef(name, fds)
      interp(subst(arg, fd.arg, fd.body), fds)
    | idC(s :: String) =>
      raise("free variable: " + s + " not assigned a value")
  end
end
   
|#

fun lookup(s :: String, nv :: Environment) -> Value:
  cases (List) nv:
    | empty => raise("unbound identifier: " + s)
    | link(f, r) =>
      if s == f.name:
        f.value
      else:
        lookup(s, r)
      end
  end
end


fun interp(e :: ExprC, nv :: Environment) -> Value:
  # removed return annotation of Value because fdC is not a Value!
  cases (ExprC) e:
    | numC(n) => numV(n)
    | plusC(l, r) => arith-binop(lam(x, y): x + y end, l, r, nv)
    | multC(l, r) => arith-binop(lam(x, y): x * y end, l, r, nv)
    | trueC => boolV(true)
    | falseC => boolV(false)
    | ifC(cnd, thn, els) =>
      ic = interp(cnd, nv)
      if is-boolV(ic):
        if ic.b:
          interp(thn, nv)
        else:
          interp(els, nv)
        end
      else:
        raise('not a boolean')
      end
    | idC(s) => lookup(s, nv)
    | fdC(_, _) => closV(e, nv)
    | appC(f, a) =>
      clos = interp(f, nv)
      arg-val = interp(a, nv)
      interp(clos.f.body, xtnd-env(bind(clos.f.arg, arg-val), clos.e))
  end
end

#| when lazy evaluation comes in handy

fun expensiveFun() -> ExpensiveResult:
  # use a lot of RAM
  # use a lot of CPU cycles
  # possibly hang for mga 5 mins
end


fun doSthWExpensiveRes(er :: ExpensiveResult) -> Number:
  if is-tuesday():
    2
  else:
    convertERToNum(er)
  end
end

doSthWExpensiveRes(expensiveFun())

|#


#| free identifiers: bad?

fun foo(x): x + y end

   2 + y   
   
   
   
foo(1 + 1)

(1 + 1) + y

|#

fun subst(w :: ExprC, at :: String, in :: ExprC) -> ExprC:
  cases (ExprC) in:
    | numC(n) => in
    | trueC => trueC
    | falseC => falseC
    | plusC(l, r) => plusC(subst(w, at, l), subst(w, at, r))
    | multC(l, r) => multC(subst(w, at, l), subst(w, at, r))
    | ifC(c, t, e) => ifC(subst(w, at, c), subst(w, at, t), subst(w, at, e))
    | appC(name, arg) => appC(name, subst(w, at, arg))
    | idC(s) => 
      if s == at:
        w
      else:
        in
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
      
    | idExt(s :: String) => idC(s)
    | appExt(name :: String, arg :: ExprExt) => appC(name, desugar(arg))
  end
end



fun interpString(expr :: String) -> Value:  
  doubl = fdC("double", "x", plusC(idC("x"), idC("x")))
  fds = [list: doubl]
  
  interp(desugar(parse(S.read-s-exp(expr))), fds)
end

#|
check "interp":
  
  interpString("3") is numV(3)
  interpString("(+ 1 2)") is numV(3)
  interpString("(* 1 2)") is numV(2)
  interpString("(* 1 (+ 1 2))") is numV(3)
  
  interpString("()") raises "parse"
  
  interpString("(if true 1 (+ 1 1))") is numV(1)
  interpString("(if false 1 (+ 1 1))") is numV(2)
  #interpString("(if (num= 1 2) 1 (+ 1 1))") is numV(2)
  interpString("(double 2)") is numV(4)
  interpString("(double (double (if true 2 3)))") is numV(8)
end
|#

fun s2p2d(expr):
  desugar(parse(S.read-s-exp(expr)))
end

check:
  f1 = fdC("x", plusC(idC("x"), idC("x")))
  f2 = fdC("x", appC(f1, appC(f1, idC("x"))))
  f3 = fdC("_", numC(5))
  f4 = fdC("x", s2p2d("(if x 1 0)"))
  fun i(e): interp(e, mt-env) end

  i(plusC(numC(5), appC(f2, numC(3)))) is numV(17)
  i(multC(appC(f3, numC(3)), numC(4))) is numV(20)
  i(plusC(numC(10), appC(f3, numC(10)))) is numV(10 + 5)
  i(plusC(numC(10), appC(f1, plusC(numC(1), numC(2)))))
    is numV(10 + 3 + 3)
  i(plusC(numC(10), appC(f2, plusC(numC(1), numC(2)))))
    is numV(10 + 3 + 3 + 3 + 3)
  
  f6 = fdC("x", multC(idC("x"), numC(2)))
  f7 = fdC("x", multC(idC("x"), numC(3)))
  f8 = fdC("x", ifC(falseC, f6, f7))
  src2 = appC(appC(f8, numC(3)), numC(2))
    
  interp(src2, mt-env) is numV(6)
  

end



















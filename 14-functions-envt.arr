

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

data FunDefC:
  | fdC(name :: String, arg :: String, body :: ExprC)
end



data ExprC:
  | trueC
  | falseC
  | numC(n :: Number)
  | plusC(l :: ExprC, r :: ExprC)
  | multC(l :: ExprC, r :: ExprC)
  | numEqC(l :: ExprC, r :: ExprC)
  | ifC(c :: ExprC, t :: ExprC, e :: ExprC)
  | idC(s :: String)
  | appC(name :: String, arg :: ExprC)
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
    nv,
    fds) -> Value:
  l-v = interp(l, nv, fds)
  r-v = interp(r, nv, fds)
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


fun interp(e :: ExprC, nv :: Environment, fds :: List<FunDefC>) -> Value:
  cases (ExprC) e:
    | numC(n) => numV(n)
    | trueC => boolV(true)
    | falseC => boolV(false)
    | plusC(l, r) => arith-binop(lam(x, y): x + y end, l, r, nv, fds)
    | multC(l, r) => arith-binop(lam(x, y): x * y end, l, r, nv, fds)
    | ifC(cnd, thn, els) =>
      ic = interp(cnd, nv, fds)
      if is-boolV(ic):
        if ic.b:
          interp(thn, nv, fds)
        else:
          interp(els, nv, fds)
        end
      else:
        raise('not a boolean')
      end
    | idC(s) => lookup(s, nv)
    | appC(f, a) =>
      fd = get-fundef(f, fds)
      newBinding = bind(fd.arg, interp(a, nv, fds))
      nvExtended = xtnd-env(newBinding, mt-env)
      interp(fd.body, nvExtended, fds)
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


fun get-fundef(name :: String, fds :: List<FunDefC>)
        -> FunDefC:
  cases (List<FunDefC>) fds:
    | empty => raise("couldn't find function")
    | link(f, r) =>
      if f.name == name:
        f
      else:
        get-fundef(name, r)
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
      f1 = fdC("double", "x", plusC(idC("x"), idC("x")))
      f2 = fdC("quad", "x", appC("double", appC("double", idC("x"))))
      f3 = fdC("const5", "_", numC(5))
      f4 = fdC("f4", "x", s2p2d("(if x 1 0)"))
      funs = [list: f1, f2, f3, f4]
      fun i(e): interp(e, mt-env, funs) end
    i(plusC(numC(5), appC("quad", numC(3)))) is numV(17)
    i(multC(appC("const5", numC(3)), numC(4))) is numV(20)
    i(plusC(numC(10), appC("const5", numC(10)))) is numV(10 + 5)
    i(plusC(numC(10), appC("double", plusC(numC(1), numC(2)))))
    is numV(10 + 3 + 3)
    i(plusC(numC(10), appC("quad", plusC(numC(1), numC(2)))))
    is numV(10 + 3 + 3 + 3 + 3)
end


















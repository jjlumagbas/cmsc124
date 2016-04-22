

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

#|
data FunDefC:
  | fdC(name :: String, arg :: String, body :: ExprC)
end
|#



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
  cases (ExprC) e:
    | numC(n) => numV(n)
    | trueC => boolV(true)
    | falseC => boolV(false)
    | plusC(l, r) => arith-binop(lam(x, y): x + y end, l, r, nv)
    | multC(l, r) => arith-binop(lam(x, y): x * y end, l, r, nv)
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
    | fdC(arg, body) => closV(e, nv)
    | appC(f, a) =>
      clos = interp(f, nv)
      fd = clos.f
      newBinding = bind(fd.arg, interp(a, nv))
      nvExtended = xtnd-env(newBinding, clos.e)
      interp(fd.body, nvExtended)
  end
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
  | fdC(arg :: String, body :: ExprC)
  | appC(f :: ExprC, arg :: ExprC)
end

check:
  # (([deffun x (if (num= x 0) [deffun x (* x 2)] [deffun x (* x 3)])] 3) 2) 
  f2 = fdC("x", multC(idC("x"), numC(2)))
  f3 = fdC("x", multC(idC("x"), numC(3)))
  f4 = fdC("x", ifC(falseC, f2, f3))
  src2 = appC(appC(f4, numC(3)), numC(2))
    
  interp(src2, mt-env) is numV(9)
  
  # [deffun x (* x 3)]
  f1 = fdC("x", multC(idC("x"), numC(3)))
  
  # (+ 2 ([deffun x (* x 3)] 4))
  src1 = plusC(numC(2), appC(f1, numC(4)))
  
  #[deffun x (if (num= x 0) [deffun x (* x 2)] [deffun x (* x 3)])]
  
    
  interp(f1, mt-env) is f1
  interp(f1, mt-env) is fdC("x", multC(idC("x"), numC(3)))
  
  interp(src1, mt-env) is numV(14)
    
    
  
  
end

#|
  
fun foo(x):
  if x == 0:
    lam(x): x * 2 end
  else:
    lam(x): x * 3 end
  end
end

foo(3) is (lam(x): 3 * 3 end)
  
foo(3)(2) is 9

   (lam(double): double(2) + double(3) end)(lam(x): x + x end)
   
   (let (x 1) x)
   
   (lam(x): x end)(1)
   
|#













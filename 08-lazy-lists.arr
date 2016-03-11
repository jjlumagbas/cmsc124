fun is-even(n):
  num-modulo(n, 2) == 0
end

#|
for all numbers x from 2 to n / 2
  if x is a factor of n
    then not prime

return true
|#

fun sum-even(n):
  fold(_ + _, 0, filter(is-even, range(1, n + 1)))
end

fun make-is-factor(n):
  lam(x):
    num-modulo(n, x) == 0
  end
end


  
fun is-prime(n):
  factors = filter(make-is-factor(n), range(2, num-floor(n / 2) + 1))
  is-empty(factors)
end


fun sum-primes(n):
  fold(_ + _, 0, filter(is-prime, range(2, n + 1)))
end





#|
x Create abstraction
x Delay evaluation
|#
5 + 2

#|
/ Create abstraction
/ Delay evaluation
|#
fun add-2(n):
  n + 2
end

#|
/ Create abstraction
x Delay evaluation
|#
(lam(x): x + 2 end)(5)

#|
x Create abstraction
/ Delay evaluation
|#
fun delay():
  5 + 2
end

data NumberStream:
  | lz-link(h :: Number, t :: ( -> NumberStream))
end

fun ones() -> NumberStream:
  lz-link(1, ones)
end

fun nats-from(n) -> NumberStream:
  lz-link(n, lam(): nats-from(n + 1) end)
end

fun first(st :: NumberStream) -> Number:
  cases (NumberStream) st:
    | lz-link(h, t) => h
  end
end

fun rest(st :: NumberStream) -> NumberStream:
  cases (NumberStream) st:
    | lz-link(h, t) => t()
  end
end


fun take(n, st :: NumberStream) -> List<Number>:
  if n == 0:
    empty
  else:
    link(first(st), take(n - 1, rest(st)))
  end
end

fun lz-map(f, st :: NumberStream) -> NumberStream:
  lz-link(f(first(st)), lam(): lz-map(f, rest(st)) end)
end 


fun lz-filter(f, st :: NumberStream) -> NumberStream:
  fst = first(st)
  if f(fst):
    lz-link(fst, lam(): lz-filter(f, rest(st)) end)
  else:
    lz-filter(f, rest(st))
  end
end

#|
fun lz-fold(f, b :: Number, st :: NumberStream) -> Number:
  lz-fold(f, f(first(st), b), rest(st))
end
|#



  
fun lz-is-prime(n):
  factors = take(1, lz-filter(make-is-factor(n), nats-from(2)))
  is-empty(factors)
end


fun lz-sum-primes(n):
  fold(_ + _, 0, lz-filter(is-prime, nats-from(2)))
end





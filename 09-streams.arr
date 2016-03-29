
data Stream<T>:
  | lz-link(h :: <T>, t :: ( -> Stream))
end

fun ones() -> Stream:
  lz-link(1, ones)
end

fun nats-from(n) -> Stream:
  lz-link(n, lam(): nats-from(n + 1) end)
end

fun first<T>(st :: Stream) -> <T>;
  cases (Stream<T>) st:
    | lz-link(h, t) => h
  end
end

fun rest(st :: Stream) -> Stream:
  cases (Stream) st:
    | lz-link(h, t) => t()
  end
end


fun take(n, st :: Stream) -> List<T>:;
  if n == 0:
    empty
  else:
    link(first(st), take(n - 1, rest(st)))
  end
end

fun lz-map(f, st :: Stream) -> Stream:
  lz-link(f(first(st)), lam(): lz-map(f, rest(st)) end)
end 

fun lz-map2(f, st1, st2):
  lz-link(f(first(st1), first(st2)), 
    lam(): lz-map2(f, rest(st1), rest(st2)) end)
end




fun lz-filter(f, st :: Stream) -> Stream:
  fst = first(st)
  if f(fst):
    lz-link(fst, lam(): lz-filter(f, rest(st)) end)
  else:
    lz-filter(f, rest(st))
  end
end


fun lz-sift(n, st) -> Stream:
  lz-filter(lam(x): num-modulo(x, n) <> 0 end, st)
end


fun lz-sieve(st) -> Stream:
  lz-link(first(st), lam(): lz-sieve(lz-sift(first(st), rest(st))) end)
end


fun sift(n, l) -> List<T>:
  filter(lam(x): num-modulo(x, n) <> 0 end, l)
end

fun sieve(l :: List<T>) -> List<T>:
  cases (List<T>) l:
    | empty => empty
    | link(f, r) => link(f, sieve(sift(f, r)))
  end
end

fun primes(n) -> List<T>:
  sieve(range(2, n))
end


fun fibs() -> Stream:
  lz-link(0, lam(): lz-link(1, lam(): lz-map2(_ + _, fibs(), rest(fibs())) end) end)
end




# No abstraction, no delay

check:
  y = 1 + 2
  y is 3
end

# Delay and abstraction

fun add3(num :: Number) -> Number:
  num + 3
end

add4 = lam(num :: Number) -> Number:
  num + 4
end


# Abstraction, no delay (put expression "under a lam")

(lam(a): a * a end)(2)


# Delay with no abstraction

fun thunk():
  2 * 2
end


data List<a>:
  | empty
  | link(first :: a, rest :: List<a>)
end

#|
fun list-fun(l :: List<Number>) -> ???:
  cases (List<Number>) l:
    | empty => ...
    | link(first, rest) =>
      ... first ...
      list-fun(rest)
  end
end
|#

fun length<a>(l :: List<a>) -> Number:
  cases (List<a>) l:
    | empty => 0
    | link(first, rest) =>
      1 + length(rest)
  end
end

fun contains-n(l :: List<Number>, n :: Number) -> Boolean:
  cases (List<Number>) l:
    | empty => false
    | link(first, rest) =>
      if (first == n):
        true
      else:
        contains-n(rest, n)
      end
  end
where:
  contains-n([list: 1, 2, 3], 3) is true
  contains-n([list: 1, 2, 3], 4) is false
  contains-n([list: 1, 3, 3], 3) is true
  contains-n(empty, 3) is false
end

fun plus-10(n):
  n + 10
end

fun times-15-percent(n):
  n * 1.15
end


fun times-neg-15-percent(n):
  n * 0.85
end


fun do-to-num-list(op :: (Number -> Number), l :: List<Number>) -> List<Number>:
  cases (List<Number>) l:
    | empty => empty
    | link(f, r) => link(op(f), do-to-num-list(op, r))
  end
where:
  l = [list: 85, 62, 90]
  do-to-num-list(plus-10, l) is [list: 95, 72, 100]
  do-to-num-list(times-15-percent, l) is [list: 85 * 1.15, 62 * 1.15, 90 * 1.15]
  do-to-num-list(times-neg-15-percent, l) is [list: 85 * 0.85, 62 * 0.85, 90 * 0.85]
end

fun do-to-list<a>(op :: (a -> a), l :: List<a>) -> List<a>:
  cases (List<a>) l:
    | empty => empty
    | link(f, r) => link(op(f), do-to-list(op, r))
  end
end


fun add-10(l :: List<Number>) -> List<Number>:
  cases (List<Number>) l:
    | empty => empty
    | link(f, r) => link(f + 10, add-10(r))
  end
end

fun bump-up(l :: List<Number>) -> List<Number>:
  cases (List<Number>) l:
    | empty => empty
    | link(f, r) => link(f * 1.15, bump-up(r))
  end
end

fun bump-down(l :: List<Number>) -> List<Number>:
  cases (List<Number>) l:
    | empty => empty
    | link(f, r) => link(f * 0.85, bump-down(r))
  end
end


data LetterGrade: A | B | C end

data Grade:
  | drop(reason :: String)
  | pass(letter-grade :: LetterGrade)
  | fail
end


fun num-to-grade(grade :: Number) -> Grade:
  if grade >= 90:
    pass(A)
  else if grade >= 80:
    pass(B)
  else if grade >= 70:
    pass(C)
  else:
    fail
  end
where:
  num-to-grade(90) is pass(A)
  num-to-grade(80) is pass(B)
  num-to-grade(70) is pass(C)
  num-to-grade(69) is fail
end


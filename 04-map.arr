fun do-to-list<a, b>(operation :: (a -> b), l :: List<a>) -> List<b>:
  cases (List<a>) l:
    | empty => empty
    | link(f, r) =>
      link(operation(f), do-to-list(operation, r))
  end
where:
  do-to-list(
    lam(st :: String) -> String: st + "!" end,
    [list: "Hi", "Hello", "Bye"]
  ) is 
    [list: "Hi!", "Hello!", "Bye!"]
end

fun check-type(l :: List<Number>) -> List<Number>:
  cases (List<Number>) l:
    | empty => empty
    | link(f, r) =>
      link(f + 10, check-type(r))
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


fun add-10-to-num(n :: Number) -> Number:
  n + 10
end

fun times-fifteen-percent(n :: Number) -> Number:
  n * 1.15
end


fun do-to-grades(operation :: (Number -> Number), l :: List<Number>) -> List<Number>:
  cases (List<Number>) l:
    | empty => empty
    | link(f, r) =>
      link(operation(f), do-to-grades(operation, r))
  end
where:
  do-to-grades(add-10-to-num, [list: 80, 90, 75]) is [list: 90, 100, 85]
  do-to-grades(times-fifteen-percent, [list: 80, 90, 75]) is [list: 92, 103.5, 86.25]
  do-to-grades(lam(x :: Number) -> Number: x + 1 end, [list: 80, 90, 75]) is [list: 80 + 1, 90 + 1, 75 + 1]
end

fun add-10(l :: List<Number>) -> List<Number>:
  do-to-grades(add-10-to-num, l)
end

fun grade-up(l :: List<Number>) -> List<Number>:
  do-to-grades(times-fifteen-percent, l)
end

fun grade-down(l :: List<Number>) -> List<Number>:
end



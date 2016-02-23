data LetterGrade: A | B | C end

data Grade:
  | drop(reason :: String)
  | pass(letter-grade :: LetterGrade)
  | fail
end


fun report-grade(g :: Grade) -> String:
  cases (Grade) g:
    | drop(reason) =>
      "Dropped because: " + reason
    | pass(grade) =>
      "Passed with grade: " + tostring(grade)
    | fail =>
      "Failed"
  end
end

fun bump-up(g :: Grade) -> Grade:
  cases (Grade) g:
    | drop(reason) => g
    | pass(letter-grade) =>
      cases (LetterGrade) letter-grade:
        | A => pass(A)
        | B => pass(A)
        | C => pass(B)
      end
    | fail => pass(C)
  end
where:
  bump-up(pass(A)) is pass(A)
  bump-up(pass(B)) is pass(A)
  bump-up(pass(C)) is pass(B)
  bump-up(fail) is pass(C)
  bump-up(drop("Absences")) satisfies is-drop
end

fun bump-up-lg(lg :: LetterGrade) -> LetterGrade:
  cases (LetterGrade) lg:
    | A => A
    | B => A
    | C => B
  end
end

fun bump-up-g(g :: Grade) -> Grade:
  cases (Grade) g:
    | drop(reason) => g # do nothing
    | pass(grade) => pass(bump-up-lg(grade))
    | fail => pass(C) 
  end
where:
  bump-up(pass(A)) is pass(A)
  bump-up(pass(B)) is pass(A)
  bump-up(pass(C)) is pass(B)
  bump-up(fail) is pass(C)
  bump-up(drop("Absences")) satisfies is-drop
end


# template for Point functions
#|
fun point-function(p :: Point) -> ???:
  cases (Point) p:
    | point(x, y) =>
      ... x ...
      ... y ...
    | polar(t, m) =>
      ... t ...
      ... m ...
  end
end
|#

# template for List functions
#|
fun list-fun<a>(l :: List<a>) -> ???:
  cases (List<a>) l:
    | empty => ...
    | link(f, r) =>
      ... f ...
      ... list-fun<a>(r) ...
  end
end
|#

fun sum(l :: List<Number>) -> Number:
  cases (List<Number>) l:
    | empty => 0
    | link(f, r) =>
      f + sum(r)
  end
end

fun length<a>(l :: List<a>) -> Number:
  cases (List<a>) l:
    | empty => 0
    | link(f, r) =>
      1 + length<a>(r)
  end
end

fun remove-3(l :: List<Number>) -> List<Number>:
  cases (List<Number>) l:
    | empty => empty
    | link(f, r) =>
      if not(f == 3):
        [list: f] + remove-3(r)
        # link(f, remove-3(r))
      else:
        remove-3(r)
      end
  end
where:
  remove-3([list: 1, 2, 3]) is [list: 1, 2]
  remove-3([list: 1, 2]) is [list: 1, 2]
  remove-3(empty) is empty
  remove-3([list: 3, 3, 3]) is empty
end



fun contains-n(l :: List<Number>, n :: Number) -> Boolean:
  cases (List<Number>) l:
    | empty => false 
    | link(f, r) =>
      if (f == n):
        true
      else:
        contains-n(r, n)
      end
  end
where:
  contains-n([list: 1, 2, 3], 3) is true
  contains-n(empty, 3) is false
  contains-n([list: 1, 2, 3], 4) is false
  contains-n([list: 1, 2, 3, 4, 4], 4) is true
end


data LetterGrade: A | B | C end

data Grade:
  | drop(reason :: String)
  | pass(letter-grade :: LetterGrade)
  | fail
end


fun num-to-grade-70(grade :: Number) -> Grade:
  if grade >= 90:
    pass(A)
  else if grade >= 80:
    pass(B)
  else if grade >= 70:
    pass(C)
  else:
    fail
  end
end

fun num-to-grade-60(grade :: Number) -> Grade:
  if grade >= 90:
    pass(A)
  else if grade >= 80:
    pass(B)
  else if grade >= 60:
    pass(C)
  else:
    fail
  end
end


fun num-to-grade-55(grade :: Number) -> Grade:
  if grade >= 90:
    pass(A)
  else if grade >= 80:
    pass(B)
  else if grade >= 55:
    pass(C)
  else:
    fail
  end
end

fun num-to-grade-passing(passing :: Number, grade :: Number) -> Grade:
  if grade >= 90:
    pass(A)
  else if grade >= 80:
    pass(B)
  else if grade >= passing:
    pass(C)
  else:
    fail
  end
end

fun make-num-to-grade(passing :: Number) -> (Number -> Grade): 
  lam(grade :: Number) -> Grade:
    if grade >= 90:
      pass(A)
    else if grade >= 80:
      pass(B)
    else if grade >= passing:
      pass(C)
    else:
      fail
    end
  end
where:
  l = [list: 54, 60, 85, 92]
  f = make-num-to-grade(60)
  map(f, l) is map(num-to-grade-60, l)
  
  g = make-num-to-grade(70)
  map(g, l) is map(num-to-grade-70, l)
end


fun compose(f, g):
  lam(x): f(g(x)) end
end





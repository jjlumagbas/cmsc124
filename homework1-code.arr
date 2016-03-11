
### Homework 1 (code file) ###


provide *

# Simple list functions
fun sum-of(l :: List<Number>) -> Number:
  doc: "Find the sum of a list of numbers."
  fold(_ + _, 0, l)
where:
  sum-of([list: 1, 2, 3]) is 6
end

fun alternating<A>(l :: List<A>) -> List<A>:
  doc: ```Extract every-other element from a list,
          starting with the first element. For instance,
          alternating([list: 1, 2, 3, 4]) is [list: 1, 3]```
  cases (List<A>) l:
    | empty => empty
    | link(f, r) => 
      cases (List<A>) r:
        | empty => [list: f]
        | link(ff, rr) => link(f, alternating(rr))
      end
  end
where:
  alternating([list: 1, 2, 3, 4]) is [list: 1, 3]
end

fun sum-alternating(l :: List<Number>) -> Number:
  doc: ```Sum every other number in a list,
          starting with the first element.```
  sum-of(alternating(l))
end

fun concat-alternating(l :: List<String>) -> String:
  doc: ```Concatenate together every other string in a list,
          starting with the first element.```
  fold(_ + _, "", alternating(l))
end

fun even(n):
  num-modulo(n, 2) == 0
end

fun removeOddNumbers(l :: List<Number>) -> List<Number> :
  doc: ```Return a new list with all the odd numbers 
       removed.```
  filter(even, l)
end



fun replicate(x :: Number, num :: Number) -> List<Number> :
  doc: ```Create a list with num copies of x.```
  if (num <= 0):
    empty
  else:
    [list: x] + replicate(x, num - 1)
  end
where:
  replicate(1, 5) is [list: 1, 1, 1, 1, 1]
end

fun reverse(l :: List<Number>) -> List<Number>:
  doc: ```Return a new list that is the reverse of the 
       original.```
  fold(lam(lst, el): [list: el] + lst end, empty, l)
end

# Check digit questions
    
fun is-check-digit-correct(num :: Number) -> Boolean:
  doc: ```Returns true if the last digit is the correct digit 
       using Luhn's algorithm.```
  false
end

fun right-digit(n):
  num-modulo(n, 10)
end

fun remove-right(n):
  num-floor(n / 10)
end

fun num-to-list(n):
  if (n <= 0):
    empty
  else:
    num-to-list(remove-right(n)) + [list: right-digit(n)]
  end
end


fun less-9(x):
  if x <= 9:
    x
  else:
    x - 9
  end
end

fun generate-check-digit(num :: Number) -> Number:
  doc: ```Calculate the digit to add to the end of this
       number to serve as a check digit using Luhn's 
       algorithm.```
  lst = num-to-list(num)
  
  sum-less-9 = sum-of(map(less-9, map(_ * 2, alternating(reverse(lst)))))
  right-digit((sum-less-9 + sum-alternating(lst)) * 9)
where:
  generate-check-digit(7992739871) is 3
end







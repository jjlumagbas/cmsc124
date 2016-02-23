
# Examples from January 28

# map is a pre-defined function in Pyret that is similar to doToNumList
# but more general.
#
# doToNumList's type is ((Number -> Number, List<Number> -> List<Number>)
#
# map is polymorphic.  Its type is:
# map<a,b> :: ((a -> b, List<a> -> List<b>)
# 
# Its first parameter is a function that can be applied to members of
# the list passed in as the second parameter.  The return value is a list
# where the members of the list match the return type of the function
# passed in.

fun isPositive(num :: Number) -> Boolean :
  num > 0
end

check "map":
  map(string-length, [list:"a", "ab", "abcd"]) is [list:1, 2, 4]
  map(isPositive, [list: 1, -1, 6]) is [list: true, false, true]
end

fun isPositives(lst :: List<Number>) -> List<Boolean>:
  map(isPositive,lst)
where:
  isPositives([list: 1, -1, 6]) is [list: true, false, true]
end

fun square-roots(lst :: List<Number>) -> List<Number>:
  map(num-sqrt, lst)
where:
  square-roots([list:1, 4, 9, 16]) is [list: 1, 2, 3, 4]
end

# filter is a higher-order function 
# filter<a> :: ((a -> Boolean), List<a> -> List<a>
#
# filter is used to compute a sublist of a list where some condition holds
# on the members
fun positiveSublist(lst :: List<Number>) -> List<Number>:
  filter(isPositive, lst)
where:
  positiveSublist([list: 1, -1, 6]) is [list: 1, 6]
end


fun num-filter(f :: (Number -> Boolean), l :: List<Number>) -> List<Number>:
  cases (List<Number>) l:
    | empty => empty
    | link(first, rest) =>
      if f(first):
        link(first, num-filter(f, rest))
      else:
        num-filter(f, rest)
      end
  end
where:
  num-filter(isPositive, [list: 1, -2, 3]) is [list: 1, 3]
end

# This example computes the same thing as positiveSublist except
# that it uses an anonymous function, a lambda expression, to define
# the filter function.
fun keepPositives(lst :: List<Number>) -> List<Number>:
  filter(
    lam(num :: Number) -> Boolean:
      num > 0
    end,
    lst)
where:
  keepPositives([list: 1, -1, 6]) is [list: 1, 6]

end

# fold is a function that allows us to accumulate a value over the
# items in a list.  Fold's type is:
#
# fold<a, b> :: ( (a, b -> a), a, List<b> -> a)
#
# The function that is passed to fold does the accumulation.
# The second parameter to fold is the starting value, i.e. the base case
# The 3rd parameter is the list we are accumulating over.
#
# Here, the accumulation function is +.  The starting value is 0, so 
# the sum of an empty list will be 0.  The third parameter is the list 
# of numbers that we want to sum.
fun sum(lst :: List<Number>) -> Number:
  fold(_ + _, 0, lst)
where:
  sum([list: 1, 2, 3]) is 6
  sum(empty) is 0
end

check "fold":
  # These first two examples of fold are not useful but they help us understand
  # what fold does.
  
  # This one always returns the accumulated value so we end up with just
  # the base case value.
  fold(
    lam(acc, cur) : acc end,
    1,
    [list: 2, 3, 4]) is 1
  
  # This one always returns the current list value so we end up with the
  # last value in the list.
  fold(
    lam(acc, cur) : cur end,
    1,
    [list: 1, 2, 3, 4]) is 4
  
  # This is equivalent to the earlier sum example except that we wrote
  # the addition function ourselves this time.
  fold(
    lam(acc, cur) : acc + cur end,
    1,
    [list: 1, 2, 3, 4]) is 11
  
  # This one counts the number of elements in a list since we start at 0
  # and add one for each list member.
  fold(
    lam(acc, cur) : acc + 1 end,
    0,
    [list: 1, 2, 3, 10]) is 4
end

fun mul(a, b): a * b end

# Demonstrating that we cannot do exact comparisons of floating point values.
# Pyret provides is% and within to help us do comparisons within epsilon.
check "sqrt":
  num-sqrt(2) is 1.414
  num-sqrt(2) is%(within(0.01)) 1.414
end

fun my-is(a, b ) -> Boolean:
  a == b
end

# addTo is a function that returns a function.  When we call it we will
# get back a function that can add one specific value to another number,
# where that specific value is the parameter that was passed in for x
# when addTo is called.
fun addTo( x :: Number ) -> (Number -> Number):
  lam(num :: Number) -> Number:
    # Notice that x is the parameter to addTo.  The function that is returned
    # will have a specific value substituted in for x.
    x + num
  end
where:
  # We can define a use a function created by addTo
  add3 = addTo(3)
  add3(6) is 9
  
  # Or we can define and use it all at once.
  addTo(3)(6) is 9
end

# This is effectively the function that addTo(3) returns.
fun addThree(num :: Number) -> Number:
  3 + num
where:
  addThree(6) is 9
end




# Examples from Jan. 21, 2016

# Creating a data type with 2 constructors that allow us
# to represent a point as either a Cartesian coordinate (x, y)
# or as a Polar coordinate.
data Point:
  | point(x :: Number, y :: Number)
  | polar(magnitude :: Number, angle :: Number)
end

# template for Point functions
#|
fun point-function(p :: Point) -> ???:
  cases (Point) p:
    | point(x, y) =>
      ... x ...
      ... y ...
    | polar(m, a) =>
      ... m ...
      ... a ...
  end
end
|#

fun convert(p :: Point) -> Point:
  doc: "Converts a point to a Cartesian point"
  
  # Notice how we use "cases" to distinguish values based
  # on how they have been constructed.
  cases (Point) p:
    | point(x, y) => p  # Nothing to do.  It is already Cartesian
    | polar(mag, angle) =>
      # Apply some trig to do the conversion
      point(mag * num-cos(angle), mag * num-sin(angle))
  end
end

# In this function, we only ever calculate a distance if both
# points are Cartesian.  We convert Polar coordinates before
# computing the distance.
fun distance(p1 :: Point, p2 :: Point) -> Number :
  doc: "Calculate the distance between 2 points."
  cases(Point) p1 :
    | point(x,y) =>
      
      cases(Point) p2 :
        | point(x2, y2) =>
          # In this case, point points are Cartesian
          xDist = p1.x - p2.x
          yDist = p1.y - p2.y
          num-sqrt((xDist * xDist) + (yDist * yDist))
          
        | polar(mag, angle) =>
          # First point is Cartesian, second is polar
          distance(p1, convert(p2))
      end
    | polar(mag, angle) =>
      # First is polar, second might be either Cartesian or polar
      distance(convert(p1), p2)
      
  end
      
where:
  distance(point(0, 0), point(0, 2)) is 2
  distance(point(0, 0), polar(2, 0)) is 2
  distance(polar(0, 0), polar(2, 0)) is 2
end

fun aliasPlay() -> Boolean:
  x = 3
  y = x
  # We cannot re-assign to a variable.  It does not compile if next 
  # line is uncommented.
  #y = 5
  if (x == y): true
  else: false
  end
end

# The following are examples of using Pyret's list type.  Recall that
# it is defined as:
# data List<a>:
#    | empty
#    | link(value :: a, rest :: List<a>
# end

fun add-one(lst :: List<Number>) -> List<Number> :
  doc: "Add 1 to each element in a list of numbers."
  cases(List<Number>) lst:
    | empty => empty  # Nothing to do on an empty list
    | link(first, rest) =>
      # Add 1 to the current value and recurse on the rest of the list.
      link(first + 1, add-one(rest))
  end
where:
  add-one(empty) is empty
  add-one([list: 1, 2, 3]) is [list: 2, 3, 4]
end

fun length<a>(lst :: List<a>) -> Number:
  doc: "Calculate the length of the list."
  cases(List<a>) lst:
    | empty => 0  # An empty list has length 0
    | link(first, rest) =>
      # A non-empty list is 1 longer that the list with the first
      # element removed.
      1 + length(rest)
  end
where:
  length(empty) is 0
  length([list: 1, 2, 3]) is 3
  length([list: "abc", "def"]) is 2
end

fun contains<a>(lst :: List<a>, val :: a) -> Boolean:
  doc: "Determine if the value is in the list."
  cases(List<a>) lst:
    | empty => false  # Nothing is in an empty list
    | link(first, rest) =>
      # Check if it is the first think in the list
      if (first == val): true  
        
        # If it is not the first thing in the list, recursively
        # check the rest of the list
      else: contains(rest, val)
      end
  end
where:
  contains(empty, 3) is false
  contains([list: 1, 2, 3], 3) is true
  contains([list: 1, 2, 3], 4) is false
  
end

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

    

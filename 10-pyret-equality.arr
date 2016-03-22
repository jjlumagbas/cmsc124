
check "list": 
  l1 = [list: 1]
  l2 = l1
  
  l1 is l2
  l1 == l2 is true
  identical(l1, l2) is true
  l1 is%(identical) l2
  
  l1 is%(equal-always) l2
  equal-always(l1, l2) is true
  
  l1 is%(equal-now) l2
  
  l3 = [list: 1]
  
  l3 is l1
  l3 == l1 is true
  l3 is-not%(identical) l1
  identical(l1, l3) is false
  
  l3 is%(equal-always) l1
  equal-always(l3, l1) is true
  
  l3 is%(equal-now) l1
 
end

data Box: box(ref v) end

check "box":
  b1 = box(1)
  b2 = b1
  
  # b1!{v: 2}
  
  b1 is b2
  b1 == b2 is true
  b1 is%(identical) b2
  identical(b1, b2) is true
  
  b1 is%(equal-always) b2
  equal-always(b1, b2) is true
  
  b1 is%(equal-now) b2
  
  b3 = box(1)
  
  b3 is-not b1
  b3 == b1 is false
  
  b3 is-not%(identical) b1
  identical(b3, b1) is false
  
  b3 is-not%(equal-always) b1
  equal-always(b3, b1) is false
  
  b3 is%(equal-now) b1
end

check "list of boxes":
  b0 = box("a value")
  b1 = box("a value")
  b2 = b1
  l0 = [list: b0]
  
  l1 = [list: b1]
  l2 = [list: b2]
  
  equal-now(l0, l1) is true
  equal-always(l0, l1) is false
  identical(l0, l1) is false
  
  equal-now(l1, l2) is true
  equal-always(l1, l2) is true
  identical(l1, l2) is false
 
end

# We next looked more at what we mean by equality and ended
# up with three notions of equality:
# -- identical - same object

# -- equal-always - all values in the structure are equivalent and mutable portions are identical
#    
# -- equal-now - all values in the structure are equivalent but mutable portions might not be identical
#    
#


data Point: point(x, y) end

data Circle: circle(p :: Point, r) end

fun move(p, x, y):
  point(p.x + x, p.y + y)
end







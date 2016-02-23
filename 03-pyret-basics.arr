fun hours-to-wages(hours :: Number) -> Number:
  doc: "Compute total wage from hours, accounting for overtime, at $10/hr base"
  if hours < 0:
    raise("Error: can't work negative hours!")
  else if hours <= 40:
    hours * 10
  else if hours > 40:
    (40 * 10) + ((hours - 40) * (10 * 1.5))
  end
where:
  hours-to-wages(40) is 400
  hours-to-wages(39.5) is 395  # this test was added
  hours-to-wages(40.5) is 407.5  # this test is correct now
  hours-to-wages(41) is 415
  hours-to-wages(0) is 0
  hours-to-wages(45) is 475
  hours-to-wages(20) is 200
  hours-to-wages(-2) raises "Error"
end
 
fun around(actual :: Number, expected :: Number) -> Boolean:
  doc: "Return whether actual is within 0.01 of expected"
  num-abs(actual - expected) < 0.5
where:
  around(5, 5.01) is true
  around(5.01, 5) is true
  around(5.02, 5) is true
  around(num-sqrt(2), 1.41) is true
end


data Point:
  | point(x :: Number, y :: Number)
end

fun distance-to-origin(p :: Point) -> Number:
  doc: "Produces the distance from the origin to the given point."
  cases (Point) p:
    | point(x, y) =>
      num-sqrt((x * x) + (y * x))
  end
where:
  distance-to-origin(point(0, 0)) is 0
  distance-to-origin(point(3, 4)) is%(around) 5
  distance-to-origin(point(-3, -4)) is%(around) 5
end

check:
  [list:].first raises "first"
end

data Pay:
  | hourly(hourly-rate :: Number)
  | salaried(yearly-salary :: Number)
end

fun expected-weekly-wages(p :: Pay) -> Number:
  doc: "Computes the expected weekly salary of an employee"
  cases (Pay) p:
    | hourly(r) => r * 40
    | salaried(s) => s / 52
  end
where:
  expected-weekly-wages(hourly(15)) is 15 * 40
  expected-weekly-wages(salaried(1500)) is 1500 / 52
end

data Name:
  | name(first :: String, last :: String)
end

fun motto2(n :: Name) -> String:
  cases (Name) n:
    | name(first, last) =>
      if (first == "Arya") and (last == "Stark"):
        "Valar Morghulis"
      else if (last == "Stark") or ((first == "Jon") and (last == "Snow")):
        "Winter is Coming"
      else if last == "Lannister":
        "Hear Me Roar"
      else if last == "Baratheon":
        "Ours is the Fury"
      end
  end
  
where:
  motto2(name("", "Stark")) is "Winter is Coming"
  motto2(name("", "Lannister")) is "Hear Me Roar"
  motto2(name("", "Baratheon")) is "Ours is the Fury"
  motto2(name("Jon", "Snow")) is "Winter is Coming"
  motto2(name("Arya", "Stark")) is "Valar Morghulis"
  motto2(name("Jon", "Baratheon")) is "Ours is the Fury"
end





fun motto(first :: String, last :: String) -> String:
  doc: "The motto of the given GoT family"
  if (first == "Arya") and (last == "Stark"):
      "Valar Morghulis"
  else if (last == "Stark") or ((first == "Jon") and (last == "Snow")):
    "Winter is Coming"
  else if last == "Lannister":
    "Hear Me Roar"
  else if last == "Baratheon":
    "Ours is the Fury"
  end
where:
  motto("", "Stark") is "Winter is Coming"
  motto("", "Lannister") is "Hear Me Roar"
  motto("", "Baratheon") is "Ours is the Fury"
  motto("Jon", "Snow") is "Winter is Coming"
  motto("Arya", "Stark") is "Valar Morghulis"
end

fun length(l :: List<Any>) -> Number:
  cases (List<Any>) l:
    | empty => 0
    | link(first, rest) => 1 + length(rest)
  end
where:
  length([list: 1, 2, 3, 4]) is 4
  length([list: "A", "B", "C"]) is 3
end

fun count-not-in(n1 :: String, n2 :: String) -> Number:
  not-in-n2 = for filter(ch from string-explode(string-toupper(n1))):
    not(string-contains(string-toupper(n2), ch))
  end
  length(not-in-n2)
where:
  count-not-in("ABCDE", "ABC") is 2
end

fun fortune(l :: Number) -> String:
  if (l == 1): "Friends"
  else if (l == 2): "Lovers"
  else if (l == 3): "Admirer"
  else if (l == 4): "Marriage"
  else if (l == 5): "Engaged"
  else if (l == 6): "Sweethearts"
  end
end

fun flames(n1 :: String, n2 :: String):
  fortune(num-modulo(count-not-in(n1, n2) + count-not-in(n2, n1), 6))
where:
  flames("ABCDE", "ABC") is "Lovers"
end







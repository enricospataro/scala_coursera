
val m = Map('k' -> 5, 'a' -> 15, 'x' -> 50)

def f(m: Map[Char,Int]): Map[Char,Int] = for {
  (k,v) <- m
} yield { (k -> v) }

m - 'k'


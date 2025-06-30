package exercises.chap2

object CurryCooking:

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  @main def runCurry(): Unit =
    val tripleCurry = curry[Int, Int, Int](_ + _)
    println(tripleCurry)
    println(tripleCurry(3)(4))
    println(tripleCurry(3)(3))
    println(uncurry(tripleCurry))
    println(uncurry(tripleCurry)(3, 4))

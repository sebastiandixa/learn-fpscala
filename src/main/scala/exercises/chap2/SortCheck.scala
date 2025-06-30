package exercises.chap2

object SortCheck:

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =

    @annotation.tailrec
    def loop(n: Int, as: Array[A]): Boolean =
      if n + 1 >= as.length then true
      else if gt(as(n), as(n + 1)) then false
      else loop(n + 1, as)

    loop(0, as)

  @main def runIsSorted(): Unit =
    println(isSorted(Array(1, 2, 3), _ > _))
    println(isSorted(Array(1, 2, 0), _ > _))
    println(isSorted(Array(3, 2, 1), _ < _))
    println(isSorted(Array("a", "b", "c"), _ > _))

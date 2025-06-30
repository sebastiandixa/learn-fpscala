import exercises.chap3.FpList
import exercises.chap3.FpList.*

// Most specifict case will match
@main def matchList(): Unit =
  val result = FpList(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  println(result)

  println(FpList.init(FpList(1, 2, 3, 4)))

  println(foldRight(FpList(1, 2, 3), Nil: FpList[Int], Cons(_, _)))

  println(flatMap(FpList(1, 2, 3), i => FpList(i, i)))

  println(addElements(FpList(1, 2, 3), FpList(4, 5, 6)))
  println(addElementsGeneric(FpList(1, 2, 3), FpList(4, 5, 6), (a, b) => a.toString + b.toString))
  println(addElementsGeneric(FpList("foo", "bar", "baz"), FpList("baz", "bar", "foo"), _ ++ _))

package exercises.chap3

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def fold[B](acc: (B, B) => B, f: A => B): B = this match
    case Leaf(a)      => f(a)
    case Branch(l, r) => acc(l.fold(acc, f), r.fold(acc, f))

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  def foldSize: Int = fold(1 + _ + _, a => 1)

  def depth: Int = this match
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + l.depth.max(r.depth)

  def foldDepth: Int = fold((a, b) => 1 + a.max(b), a => 0)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def foldMap[B](f: A => B): Tree[B] = fold(Branch(_, _), a => Leaf(f(a)))

object Tree:

  extension (t: Tree[Int])

    def maximum: Int =
      t match
        case Leaf(i)      => i
        case Branch(l, r) => l.maximum.max(r.maximum)

  extension (t: Tree[Int])
    def maximumFold: Int = t.fold((a, b) => a.max(b), a => a)

  @main def runTrees(): Unit =
    val t = Branch(Branch(Leaf("A"), Branch(Leaf("B"), Leaf("C"))), Branch(Branch(Leaf("D"), Leaf("E")), Leaf("F")))
    println(t)
    println(t.depth)
    println(t.foldDepth)
    println(t.size)
    println(t.foldSize)

    val tint = t.map(s => Integer.parseInt(s, 16))
    println(tint)
    println(tint.maximum)
    println(tint.depth)
    println(t.size)

    val foldTint = t.foldMap(s => Integer.parseInt(s, 16))
    println(foldTint)
    println(foldTint.maximumFold)
    println(foldTint.foldDepth)
    println(foldTint.foldSize)

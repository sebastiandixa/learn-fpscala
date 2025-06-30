package exercises.chap3

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + l.depth.max(r.depth)

object Tree:

  extension (t: Tree[Int])

    def maximum: Int =
      t match
        case Leaf(i)      => i
        case Branch(l, r) => l.maximum.max(r.maximum)

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l, f), map(r, f))

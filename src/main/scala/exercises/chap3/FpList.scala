package exercises.chap3

enum FpList[+A]:
    case Nil
    case Cons(head: A, tail: FpList[A])

object FpList:
    def apply[A](as: A*): FpList[A] =
        if as.isEmpty then Nil
        else Cons(as.head, apply(as.tail*))

    def sum(ints: FpList[Int]): Int = ints match
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)

    def tail[A](as: FpList[A]): FpList[A] =
        as match
            case Nil => sys.error("Empty list")
            case Cons(x, xs) => xs

    def setHead[A](as: FpList[A], h: A): FpList[A] =
        as match
            case Nil => Cons(h, Nil)
            case Cons(x, xs) => Cons(h, xs)

    def drop[A](as: FpList[A], n: Int): FpList[A] =
        if n <= 0 then as
        else
            as match
                case Nil => Nil
                case Cons(x, xs) => drop(xs, n - 1)

    def dropWhile[A](as: FpList[A], f: A => Boolean): FpList[A] =
        as match
            case Nil => Nil
            case Cons(x, xs) => 
                if f(x) then dropWhile(xs, f)
                else as

    def init[A](as: FpList[A]): FpList[A] =
        as match
            case Nil => sys.error("Nothing to remove")
            case Cons(x, Nil) => Nil
            case Cons(x, xs) => Cons(x, init(xs))

    def foldRight[A, B](as: FpList[A], acc: B, f: (A, B) => B): B =
        as match
            case Nil => acc
            case Cons(x, xs) => f(x, foldRight(xs, acc, f))

    def lengthRight[A](as: FpList[A]): Int =
        foldRight(as, 0, (_, acc) => acc + 1)

    def foldLeft[A, B](as: FpList[A], acc: B, f: (B, A) => B): B =
        as match
            case Nil => acc
            case Cons(x, xs) => foldLeft(xs, f(acc, x), f)
    
    def sumLeft(ints: FpList[Int]): Int =
        foldLeft(ints, 0, _ + _)

    def product(ds: FpList[Double]): Double =
        foldLeft(ds, 1.0, _ * _)

    def lengthLeft[A](as: FpList[A]): Int =
        foldLeft(as, 0, (acc, _) => acc + 1)

    def reverse[A](as: FpList[A]): FpList[A] =
        foldLeft(as, Nil: FpList[A], (acc, a) => Cons(a, acc))

    def foldRightUsingLeft[A, B](as: FpList[A], acc: B, f: (A, B) => B): B =
        foldLeft(reverse(as), acc, (b, a) => f(a, b))

    def append[A](xs: FpList[A], ys: FpList[A]): FpList[A] =
        foldRight(xs, ys, Cons(_, _))
    
    def concat[A](as: FpList[FpList[A]]): FpList[A] =
        foldRight(as, Nil: FpList[A], append)

    def incrementEach(ints: FpList[Int]): FpList[Int] =
        foldRight(ints, Nil: FpList[Int], (i, acc) => Cons(i + 1, acc))

    def doubleToString(ds: FpList[Double]): FpList[String] =
        foldRight(ds, Nil: FpList[String], (d, acc) => Cons(d.toString, acc))

    def map[A, B](as: FpList[A], f: A => B): FpList[B]
        foldRight(as, Nil: FpList[B], (a, acc) => Cons(f(a), acc))

    def filter[A, B](as: FpList[A], f: A => Boolean): FpList[A]
        foldRight(as, Nil: FpList[A], (a, acc) => if f(a) Cons(a, acc) else acc)
        
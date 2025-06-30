import exercises.chap3.FpList
import exercises.chap3.FpList.*

def hasSubsequence[A](seq: FpList[A], sub: FpList[A]): Boolean =
    @annotation.tailrec
    def loop[A](l: FpList[A], pfx: FpList[A]): Boolean =
        (l, pfx) match
            case (_, Nil) => true
            case (Cons(h, t), Cons(subh, subt)) if h == subh => loop(t, subt)
            case _ => false

    seq match
        case Nil => sub == Nil
        case _ if loop(seq, sub) => true
        case Cons(h, t) => hasSubsequence(t, sub)
        

@main def matchSubsequences(): Unit =
  println(hasSubsequence(FpList(1, 2, 3, 4), FpList(1, 2)))
  println(hasSubsequence(FpList(1, 2, 3, 4), FpList(1, 3)))
  println(hasSubsequence(FpList(1, 2, 3, 4), FpList(4)))
  println(hasSubsequence(FpList(1, 2, 3, 4), FpList(2, 3)))

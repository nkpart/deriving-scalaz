package ds

trait FoldLeft[F[_]] {
  def foldLeft[B, A](t: F[A], b: B, f: (B, A) => B): B
}

object FoldLeft {
  implicit object ListFoldLeft extends FoldLeft[List] {
    def foldLeft[B, A](t: List[A], b: B, f: (B, A) => B): B = t.foldLeft(b)(f)
  }
  
  implicit object OptionFoldLeft extends FoldLeft[Option] {
    def foldLeft[B, A](t: Option[A], b: B, f: (B, A) => B): B = t match {
      case Some(x) => f(b, x)
      case None => b
    }
  }
}

trait Monoid[T] {
  def mappend(a: T, b: T): T
  def mzero: T
}

object Monoid {
  implicit object IntMonoid extends Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero: Int = 0
  }

  implicit object StringMonoid extends Monoid[String] {
    def mappend(a: String, b: String): String = a + b
    def mzero: String = ""
  }
}

object Main {
  implicit def toIdent[A](a: A) = new Identity[A] { val value = a }
  
  def sum[T, M[_]](xs: M[T])(implicit m: Monoid[T], f: FoldLeft[M]): T = f.foldLeft(xs, m.mzero, m.mappend)
  
//  def sum[T](xs: List[T]): T = ...
//  def sum[M[_], T](xs: M[T]): T = ...

  def plus[T](a: T, b: T)(implicit m: Monoid[T]): T = m.mappend(a, b)
  
  
  def p(a: Any) { println("###> " + a) }
  def main(args: Array[String]) {
    println
    
    p(3.plus(4))
    
    p(List(1,2,3).sum)
    
                //     
                // p(sum(List(1,2,3,4)))
                // p(sum(List("a","b")))
                // p(sum(Some(5): Option[Int]))
                // p(sum(None: Option[Int]))
    println
  }
}
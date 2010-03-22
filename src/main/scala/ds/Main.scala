package ds

trait FoldLeft[F[_]] {
  def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
}

object FoldLeft {
  implicit object FoldLeftList extends FoldLeft[List] {
    def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B): B = xs.foldLeft(b)(f)
  }
}

trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A   
}

object Monoid {
  implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero: Int = 0
  }
  
  implicit object StringMonoid extends Monoid[String] {
    def mappend(a: String, b: String): String = a + b
    def mzero: String = ""
  }
}

trait Identity[A] {
  val value: A
  
  def |+|(a2: A)(implicit m: Monoid[A]): A = m.mappend(value, a2)
  
  
}

trait MA[M[_], A] {
  val value: M[A]
  
  def sum(implicit m: Monoid[A], fl: FoldLeft[M]): A =
    fl.foldLeft(value, m.mzero, m.mappend)
    
  
}

object Main {
  implicit def toIdentity[A](a: A): Identity[A] = new Identity[A] {
    val value = a
  }
  
  implicit def toMA[M[_], A](ma: M[A]): MA[M,A] = new MA[M,A] {
    val value = ma
  }
  
  val multMonoid = new Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a * b
    def mzero: Int = 1
  }
  
  def p(a: Any) { println("###> " + a) }
  def main(args: Array[String]) {
    println
    
    p(3 |+| 4)
    p(List(1,2,3).sum)
    
    
    // p(sum(List(1,2,3,4)))
    //     p(sum(List("a", "b", "c")))
    //     p(sum(List(1,2,3,4))(multMonoid, implicitly[FoldLeft[List]]))
    //     
    println
  }
}
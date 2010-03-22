package ds

trait Identity[A] {
  val value: A
  
  def plus(b: A)(implicit m: Monoid[A]): A = m.mappend(value, b)
}

trait MA[M[_], A] {
  val value: M[A]
  def sum(implicit m: Monoid[A], f: FoldLeft[M]): A = f.foldLeft(value, m.mzero, m.mappend)
}

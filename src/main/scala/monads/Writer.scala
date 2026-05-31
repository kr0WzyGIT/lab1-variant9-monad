package monads

trait LogMonoid[L]:
  def empty: L
  def combine(a: L, b: L): L

object LogMonoid:
  given LogMonoid[Vector[String]] with
    override def empty: Vector[String] = Vector.empty
    override def combine(a: Vector[String], b: Vector[String]): Vector[String] = a ++ b

  given LogMonoid[List[String]] with
    override def empty: List[String] = Nil
    override def combine(a: List[String], b: List[String]): List[String] = a ++ b

final case class Writer[Log, A](log: Log, value: A):
  def map[B](f: A => B): Writer[Log, B] =
    Writer(log, f(value))

  def flatMap[B](f: A => Writer[Log, B])(using lm: LogMonoid[Log]): Writer[Log, B] =
    val next = f(value)
    Writer(lm.combine(log, next.log), next.value)

object Writer:
  def tell[Log](entry: Log): Writer[Log, Unit] =
    Writer(entry, ())

  def pure[Log, A](value: A)(using lm: LogMonoid[Log]): Writer[Log, A] =
    Writer(lm.empty, value)

  given [Log](using lm: LogMonoid[Log]): Monad[[A] =>> Writer[Log, A]] with
    override def pure[A](value: A): Writer[Log, A] =
      Writer.pure(value)

    override def flatMap[A, B](fa: Writer[Log, A])(f: A => Writer[Log, B]): Writer[Log, B] =
      fa.flatMap(f)

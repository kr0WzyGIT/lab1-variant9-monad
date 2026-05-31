package monads

final case class IO[A](thunk: () => A):
  def map[B](f: A => B): IO[B] =
    IO(() => f(thunk()))

  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(() => f(thunk()).thunk())

  def unsafeRun(): A = thunk()

object IO:
  def pure[A](value: A): IO[A] =
    IO(() => value)

  def delay[A](effect: => A): IO[A] =
    IO(() => effect)

  def putStrLn(value: String): IO[Unit] =
    delay(println(value))

  def readLine: IO[String] =
    delay(scala.io.StdIn.readLine())

  given Monad[IO] with
    override def pure[A](value: A): IO[A] =
      IO.pure(value)

    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
      fa.flatMap(f)

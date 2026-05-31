package monads

final case class Reader[Env, A](run: Env => A):
  def map[B](f: A => B): Reader[Env, B] =
    Reader(env => f(run(env)))

  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(run(env)).run(env))

object Reader:
  def ask[Env]: Reader[Env, Env] = Reader(identity)

  def pure[Env, A](value: A): Reader[Env, A] =
    Reader(_ => value)

  given [Env]: Monad[[A] =>> Reader[Env, A]] with
    override def pure[A](value: A): Reader[Env, A] =
      Reader.pure(value)

    override def flatMap[A, B](fa: Reader[Env, A])(f: A => Reader[Env, B]): Reader[Env, B] =
      fa.flatMap(f)

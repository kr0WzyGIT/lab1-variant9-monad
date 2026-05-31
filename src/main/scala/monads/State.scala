package monads

final case class State[S, A](run: S => (S, A)):
  def map[B](f: A => B): State[S, B] =
    State(s =>
      val (next, value) = run(s)
      (next, f(value))
    )

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s =>
      val (next, value) = run(s)
      f(value).run(next)
    )

object State:
  def pure[S, A](value: A): State[S, A] =
    State(s => (s, value))

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](newState: S): State[S, Unit] =
    State(_ => (newState, ()))

  def modify[S](f: S => S): State[S, Unit] =
    State(s => (f(s), ()))

  given [S]: Monad[[A] =>> State[S, A]] with
    override def pure[A](value: A): State[S, A] =
      State.pure(value)

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      fa.flatMap(f)

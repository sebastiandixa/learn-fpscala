package catseffecttutorial.producerconsumer.exercise

import cats.effect._
import cats.effect.std.Console
import cats.instances.list._
import cats.syntax.all._
import cats.effect.syntax.all._
import scala.collection.immutable.Queue

object MyQueueConsumerProducer extends IOApp {

  def producer[F[_]: Async: Console](id: Int, counterR: Ref[F, Int], queue: MyQueue[F, Int]): F[Unit] = {
    for {
      i <- counterR.getAndUpdate(_ + 1)
      _ <- queue.offer(i)
      _ <- Async[F].whenA(i % 100000 == 0)(Console[F].println(s"Producer $id has reached $i items"))
      _ <- producer(id, counterR, queue)
    } yield ()
  }

  def consumer[F[_]: Async: Console](id: Int, queue: MyQueue[F, Int]): F[Unit] = {

    for {
      i <- queue.take
      _ <- Async[F].whenA(i % 100000 == 0)(Console[F].println(s"Consumer $id has reached $i items"))
      _ <- consumer(id, queue)
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      queue    <- MyQueue[IO, Int](200)
      counterR <- Ref.of[IO, Int](1)
      producers = List.range(1, 11).map(producer(_, counterR, queue)) // 10 producers
      consumers = List.range(1, 21).map(consumer(_, queue))           // 20 consumers
      res <- (producers ++ consumers)
        .parSequence.as(ExitCode.Success)
        .handleErrorWith { t =>
          Console[IO].errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
        }
    } yield res
}

package catseffecttutorial

import cats.effect._
import java.io._
import scala.concurrent.duration.DurationInt
import cats.effect.std.Console
import scala.math.Numeric.Implicits.infixNumericOps

object Main extends IOApp {

  def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO.blocking(new FileInputStream(f))
    } { inStream =>
      new File("releasing-inputstream-ok.txt").createNewFile()
      IO.blocking(inStream.close()).handleErrorWith(_ => IO.unit) // release
    }

  def outputStream(f: File): Resource[IO, FileOutputStream] =
    Resource.make {
      IO.blocking(new FileOutputStream(f))
    } { outStream =>
      new File("releasing-outputStream-ok.txt").createNewFile()
      IO.blocking(outStream.close()).handleErrorWith(_ => IO.unit)
    }

  def inputOutputStreams(in: File, out: File): Resource[IO, (InputStream, OutputStream)] =
    for {
      inStream  <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)

  def transfer(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
    for {
      amount <- IO.blocking(origin.read(buffer, 0, buffer.length))
      count <- if (amount > -1)
        IO.blocking(destination.write(buffer, 0, amount)) >> transfer(origin, destination, buffer, acc + amount)
      else IO.pure(acc)
      _ <- IO.sleep(5.second)
    } yield count // Returns the actual amount of bytes transferred

  def transferDir(in: File, out: File): IO[Long] =
    (in.listFiles().filter(!_.isDirectory).map(f => copy(f, new File(out, f.getName))) ++ in.listFiles().filter(
      _.isDirectory
    ).map(f => transferDir(f, new File(out, f.getName)))).reduceLeft(_ + _)

  def copy(origin: File, destination: File): IO[Long] =
    if origin.isDirectory then
      transferDir(origin, destination)
    else
      inputOutputStreams(origin, destination).use { case (in, out) =>
        transfer(in, out, new Array[Byte](1024 * 10), 0)
      }

  def checkOverwrite(): IO[Unit] =
    for {
      _ <- IO.println("Destination file already exists. Overwrite y/n?")
      n <- Console[IO].readLine
      _ <- IO.raiseWhen(n.toLowerCase != "y")(new IllegalArgumentException("Will not overwrite destination file"))
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO.raiseWhen(args.length < 2)(new IllegalArgumentException("Need origin and destination files"))
      _ <- IO.raiseWhen(args(0) == args(1))(
        new IllegalArgumentException("Origin and destination files cannot be the same")
      )
      orig = new File(args(0))
      _ <- IO.raiseWhen(!orig.canRead)(new IllegalArgumentException("Origin cannot be read"))
      dest = new File(args(1))
      _     <- IO.raiseWhen(!dest.canWrite)(new IllegalArgumentException("Cannot write to destination"))
      _     <- if dest.exists then checkOverwrite() else IO.unit
      count <- copy(orig, dest)
      _     <- IO.println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}")
    } yield ExitCode.Success
}

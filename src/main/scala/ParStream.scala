package scalax

import java.lang.Runnable
import java.util.concurrent.{
  ArrayBlockingQueue,
  BlockingQueue,
  Executors,
  ThreadPoolExecutor,
  TimeUnit,
  RejectedExecutionException
}

import scala.actors.{Actor, TIMEOUT}
import scala.actors.scheduler.ExecutorScheduler
import scala.collection.GenTraversableOnce

// TODO Handle Empty PView in a kosher way
// TODO Handle toString on PView (empty/full) like iterator

object ParStream {
  def apply[A](_iterator: Iterator[A], _poolSize: Int) = new ParStream[A] {
    override lazy val iterator = _iterator
    override val poolSize = _poolSize
  }

  final class ParStreamable[+A](iterator: Iterator[A]) {
    def parstream = ParStream[A](iterator, Runtime.getRuntime.availableProcessors)
    def parstreamBy(poolSize: Int) = ParStream[A](iterator, poolSize)
  }

  implicit def iteratorPimp[A](iterator: Iterator[A]) = new ParStreamable[A](iterator)
  implicit def iterablePimp[A](iterable: Iterable[A]) = iteratorPimp(iterable.iterator)
  implicit def parstream2iterator[A](parstream: ParStream[A]) = parstream.iterator
}


trait ParStream[+A] {
  val iterator: Iterator[A]
  val poolSize: Int

  def hasNext: Boolean = iterator.hasNext
  def next(): A = iterator.next

  def foreach[U](f: A =>  U) =
    async[Nothing]((i, o) => f(i))
 
  def map[B](f: A => B) =
    async[B]((i, o) => o.add(f(i)))

  def flatMap[B](f: A => GenTraversableOnce[B]) =
    async[B]((i, o) => f(i).foreach(o.add(_)))

  protected def async[B](f: (A, { def add(o: Any) }) => Unit) = 
    new Coordinator[A, B](iterator, f).iterator

  protected final class Coordinator[I, O](input: Iterator[I], f: (I, { def add(o: Any) }) => Unit) {

    final class Generator[I, O] extends Actor {
      def isActive = getState != Actor.State.Terminated 

      def act() = {
        Coordinator.this.input.foreach((i) => executor.submit(new Runnable { def run = f(i, queue) }))
        executor.shutdown
        executor.awaitTermination(60 * 60, TimeUnit.SECONDS)
        queue ! None
      }

      val executor = Executors.newFixedThreadPool(ParStream.this.poolSize)

      start
    }

    final class Queue extends Actor {
      def add(o: Any) = this ! Some(o)
      def act() = loop(react{
        case Next => reply { receive { case x: Option[_] => x } }
        case Stop => exit()
      })
    }

    case class Add[O](o: O)
    case class Stop()
    case class Next()

    val queue = new Queue
    val generator = new Generator[I, O]
    
    val iterator: Iterator[O] = new Iterator[O] {

      def hasNext: Boolean = {
        look match {
          case Some(x) => { Coordinator.this.start; true }
          case None => { queue ! Stop; false }
        }
      }

      def next: O = look match {
        case Some(x) => current = null; x.asInstanceOf[O]
      }

      private var current: Any = null

      private def look = {
        if (current == null) current = queue !? Next
        current
      }
    }

    private var started = false

    private def start = if (!started) {
      queue.start
      generator.start
      started = true
    }
  }
}

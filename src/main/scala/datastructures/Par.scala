package datastructures
import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit};

class Par[+A]

object Par {
    type Par[A] = ExecutorService => Future[A]

    def unit[A](a : A) : Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get : A) extends Future[A] {
        def isDone = true;
        def get(timeout: Long, units: TimeUnit) = get
        def isCancelled = false;
        def cancel(evenIfRunning:Boolean) : Boolean = false
    }

    private case class Map2Future[A,B,C](a : Future[A], b : Future[B])(f : (A,B) => C) extends Future[C]{
        var cache : Option[C] = None
        def isDone = cache.isDefined
        def get(timeout: Long, units : TimeUnit) : Option[C] = compute(timeout, units )
        def isCancelled = a.isCancelled || b.isCancelled
        def cancel(evenIfRunning:Boolean) : Boolean = a.cancel(evenIfRunning) && b.cancel(evenIfRunning)

        def compute(timeout: Long, units : TimeUnit): Option[C] = {
            cache match {
                case Some(c) => Some(c)
                case None => {
                    val start = System.currentTimeMillis()
                    val af = a.get(timeout, units)
                    val mid = System.currentTimeMillis()
                    val bf = b.get(mid - start, units)
                    val ret = f(af,bf)
                    cache = Some(ret)
                    cache
                }
            }

        }
    }
    def lazyUnit[A](a : => A) : Par[A] = fork (unit(a))

    def fork[A](a : => Par[A]) : Par[A] = es => es.submit(new Callable[A]{def call = a(es).get})

    def map2[A,B,C](v1 :  Par[A], v2: Par[B])(f :( A, B) => C) : Par[C] =
        (es : ExecutorService) => {
            val v1f = v1(es)
            val v2f = v2(es)
            Map2Future(v1f, v2f)(f)
        }

    def asyncF[A,B](f: A => B) : A => Par[B] =
        (a : A) => lazyUnit(f(a))

    def run[A](s: ExecutorService)(a : Par[A]) : Future[A] = a(s)
}

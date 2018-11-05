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
        def get(timeout: Long, units : TimeUnit) : C = compute(timeout, units )
        def get() : C = get(Long.MaxValue, TimeUnit.MILLISECONDS)
        def isCancelled = a.isCancelled || b.isCancelled
        def cancel(evenIfRunning:Boolean) : Boolean = a.cancel(evenIfRunning) && b.cancel(evenIfRunning)

        def compute(timeout: Long, units : TimeUnit): C = {
            cache match {
                case Some(c) => c
                case None => {
                    val start = System.currentTimeMillis()
                    val af = a.get(timeout, units)
                    val mid = System.currentTimeMillis()
                    val bf = b.get(mid - start, units)
                    val ret = f(af,bf)
                    cache = Some(ret)
                    ret
                }
            }

        }
    }

    def delay[A](fa : => Par[A]) : Par[A] = es => fa(es)
    def lazyUnit[A](a : => A) : Par[A] = fork (unit(a))

    def fork[A](a : => Par[A]) : Par[A] = es => es.submit(new Callable[A]{def call = a(es).get})

    def map2[A,B,C](v1 :  Par[A], v2: Par[B])(f :( A, B) => C) : Par[C] =
        (es : ExecutorService) => {
            val v1f = v1(es)
            val v2f = v2(es)
            Map2Future(v1f, v2f)(f)
        }

    def map[A,B](p : Par[A])(f:A => B) : Par[B] = map2(p, unit(()))( (a,_) => f(a))

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]) : Par[IndexedSeq[A]] = fork {
        if (as.isEmpty) unit(Vector())
        else if (as.length == 1) map (as.head)(a => Vector(a))
        else {
            val (l,r) = as.splitAt(as.length/2)
            map2(sequenceBalanced(l),sequenceBalanced(r))(_ ++ _)
        }
    }

    def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]) : Boolean = {
        p1(e).get == p2(e).get
    }
    def asyncF[A,B](f: A => B) : A => Par[B] =
        (a : A) => lazyUnit(f(a))

    def run[A](s: ExecutorService)(a : Par[A]) : Future[A] = a(s)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]) : Par[A] =
    es => {
        val chosen = run(es)(n).get
        choices(chosen)(es)
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]) : Par[A] = choiceN(map(cond)(if (_) 1 else 0))(List(t,f))

    def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]) : Par[V] =
        es => {
            val chosen = run(es)(key).get
            choices(chosen)(es)
        }

    def flatMap[A,B](pa: Par[A])(choices: A => Par[B]) : Par[B] =
    es => {
        choices(run(es)(pa).get)(es)
    }

    def flatMap2[A,B](pa: Par[A])(choices: A => Par[B]) : Par[B] = join(map(pa)(choices))

    def join2[A](ppa : Par[Par[A]]) : Par[A] = flatMap(ppa)(x => x)

    def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]) : Par[A] =
        flatMap(cond)(if (_) t else f)

    def choiceMapViaChooser[K,V](key: Par[K])(choices: Map[K, Par[V]]) : Par[V] =
        flatMap(key)(choices(_))

    def join[A](ppa : Par[Par[A]]) : Par[A] = es => {
        val innerPar = (run(es)(ppa).get)
        run(es)(innerPar)
    }
}

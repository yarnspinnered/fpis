package datastructures
import java.util.concurrent.ExecutorService;

class Par[+A]

object Par {
    def unit[A](a : A) : Par[A] = ???

    def lazyUnit[A](a : => A) : Par[A] = fork (unit(a))

    def fork[A](a : Par[A]) : Par[A] = ???

    def map2[A](v1 :  A, v2: A)(f :(A,A) => A) : Par[A] = ???

    def run[A](a : Par[A]) : A = ???
}

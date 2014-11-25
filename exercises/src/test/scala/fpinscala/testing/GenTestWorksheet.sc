package fpinscala.testing

import fpinscala.state.RNG
import Gen._
import Prop._
import java.util.concurrent.{ ExecutorService, Executors }
import fpinscala.parallelism.Par

object GenTestWorksheet {

  val intList = Gen.listOf(Gen.choose(0, 100))    //> intList  : fpinscala.testing.SGen[List[Int]] = SGen(<function1>)
  val genInt = Gen.choose(0, 100)                 //> genInt  : fpinscala.testing.Gen[Int] = Gen(State(<function1>))
  val genInt2 = Gen.choose(0, 100)                //> genInt2  : fpinscala.testing.Gen[Int] = Gen(State(<function1>))

  val genTuple = for {
    a <- genInt
    b <- genInt2
  } yield (a, b)                                  //> genTuple  : fpinscala.testing.Gen[(Int, Int)] = Gen(State(<function1>))

  val genOption: Gen[Option[Int]] = genInt map (Some(_))
                                                  //> genOption  : fpinscala.testing.Gen[Option[Int]] = Gen(State(<function1>))
  val genStr = genInt map (String.valueOf(_))     //> genStr  : fpinscala.testing.Gen[String] = Gen(State(<function1>))
  val genSum = genInt.map2(genInt2)(_ + _)        //> genSum  : fpinscala.testing.Gen[Int] = Gen(State(<function1>))
  val genUnion = Gen.union(genInt, genInt2)       //> genUnion  : fpinscala.testing.Gen[Int] = Gen(State(<function1>))

  val rand = RNG.Simple(System.currentTimeMillis) //> rand  : fpinscala.state.RNG.Simple = Simple(1416715529056)
  val genRun = (g: Gen[_]) => g.sample.run(rand)._1
                                                  //> genRun  : fpinscala.testing.Gen[_] => Any = <function1>
  genRun(genUnion)                                //> res0: Any = 97

  val sgen = Gen.listOf(genInt)                   //> sgen  : fpinscala.testing.SGen[List[Int]] = SGen(<function1>)
  val sgen2 = genInt2.unsized                     //> sgen2  : fpinscala.testing.SGen[Int] = SGen(<function1>)
  val size = 10                                   //> size  : Int = 10
  //  genRun(sgen.map2(sgen2)(_ + _)(size))
  genRun(sgen(size))                              //> res1: Any = List(12, 12, 59, 6, 31, 29, 60, 90, 97, 4)
  genRun(intList(size))                           //> res2: Any = List(12, 12, 59, 6, 31, 29, 60, 90, 97, 4)
  // val

  val smallInt = Gen.choose(-10, 10)              //> smallInt  : fpinscala.testing.Gen[Int] = Gen(State(<function1>))

  // list.max property
  val maxProp = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }                                               //> maxProp  : fpinscala.testing.Prop = Prop(<function3>)
  run(maxProp)                                    //> + OK, passed 100 tests.

  // list.sorted property
  val sortedProp = forAll(listOf1(smallInt)) { l => // first element of sorted list is smallest one
    val sorted = l.sorted
    sorted.head == l.min
  } &&
    forAll(listOf(smallInt)) { l => // in every pair of neighbour elements first one is smaller or equal
      val sorted = l.sorted
      sorted.isEmpty || (sorted.size == 1) || (sorted.zip(sorted.tail) forall {
        _ match {
          case (a, b) => a <= b
        }
      })
    } &&
    forAll(listOf(smallInt)) { l => // contans all and only elements from the original list
      val sorted = l.sorted
      !sorted.exists { !l.contains(_) } &&
        !l.exists { !sorted.contains(_) }
    }                                             //> sortedProp  : fpinscala.testing.Prop = Prop(<function3>)

  run(sortedProp)                                 //> + OK, passed 100 tests.

  run(check(true))                                //> + OK, proved property.

  val ES: ExecutorService = Executors.newCachedThreadPool
                                                  //> ES  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadPoo
                                                  //| lExecutor@c038203[Running, pool size = 0, active threads = 0, queued tasks 
                                                  //| = 0, completed tasks = 0]

  val parProp2 = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }                                               //> parProp2  : fpinscala.testing.Prop = Prop(<function3>)
  run(parProp2)                                   //> + OK, proved property.

  def equalPar[A](p: Par.Par[A], p2: Par.Par[A]): Par.Par[Boolean] =
    Par.map2(p, p2)(_ == _)                       //> equalPar: [A](p: fpinscala.parallelism.Par.Par[A], p2: fpinscala.parallelis
                                                  //| m.Par.Par[A])fpinscala.parallelism.Par.Par[Boolean]
  val parProp3 = check {
    equalPar(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2))(ES).get
  }                                               //> parProp3  : fpinscala.testing.Prop = Prop(<function3>)
  run(parProp3)                                   //> + OK, proved property.

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25)   //> S  : fpinscala.testing.Gen[java.util.concurrent.ExecutorService] = Gen(Stat
                                                  //| e(<function1>))

  def forAllPar[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get } //> forAllPar: [A](g: fpinscala.testing.Gen[A])(f: A => fpinscala.parallelism.P
                                                  //| ar.Par[Boolean])fpinscala.testing.Prop

  val pint = Gen.choose(0, 10) map (Par.unit(_))  //> pint  : fpinscala.testing.Gen[fpinscala.parallelism.Par.Par[Int]] = Gen(Sta
                                                  //| te(<function1>))
  // map(x)(a => a) == n
  val p4 =
    forAllPar(pint)(n => equalPar(Par.map(n)(y => y), n))
                                                  //> p4  : fpinscala.testing.Prop = Prop(<function3>)
  run(p4)                                         //> + OK, passed 100 tests.

  val pDeepInt = Gen.choose(0, 10) map (Par.lazyUnit(_))
                                                  //> pDeepInt  : fpinscala.testing.Gen[fpinscala.parallelism.Par.Par[Int]] = Gen
                                                  //| (State(<function1>))
  val p5 =
    forAllPar(pDeepInt)(n => equalPar(Par.map(n)(y => y), n))
                                                  //> p5  : fpinscala.testing.Prop = Prop(<function3>)
  run(p5)                                         //> + OK, passed 100 tests.

  // fork(x) == n
  val p6 =
    forAllPar(pint)(n => equalPar(Par.fork(n), n))//> p6  : fpinscala.testing.Prop = Prop(<function3>)
  run(p6)                                         //> + OK, passed 100 tests.|

}
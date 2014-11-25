package fpinscala.testing

import fpinscala.state.RNG
import Gen._
import Prop._
import java.util.concurrent.{ ExecutorService, Executors }
import fpinscala.parallelism.Par
import fpinscala.errorhandling._

object GenTestFunc {

  val intList = Gen.listOf(Gen.choose(0, 100))    //> intList  : fpinscala.testing.SGen[List[Int]] = SGen(<function1>)
  val intStream = intList map (_.toStream)        //> intStream  : fpinscala.testing.SGen[scala.collection.immutable.Stream[Int]] 
                                                  //| = SGen(<function1>)
  val genInt = Gen.choose(0, 100)                 //> genInt  : fpinscala.testing.Gen[Int] = Gen(State(<function1>))
  val genInt2 = Gen.choose(0, 100)                //> genInt2  : fpinscala.testing.Gen[Int] = Gen(State(<function1>))

  val twMax = 50                                  //> twMax  : Int = 50
  val twPred = (i: Int) => i <= twMax             //> twPred  : Int => Boolean = <function1>
  val twProp = forAll(intList) { l =>
    {
      val twL = l.takeWhile(twPred)
      val dwL = l.dropWhile(twPred)
      (l.takeWhile(twPred).size <= l.size) &&
        (if (twL.isEmpty) true else twL.max <= twMax) &&
        twL.forall(twPred) &&
        twL ++ dwL == l
    }
  }                                               //> twProp  : fpinscala.testing.Prop = Prop(<function3>)
  run(twProp)                                     //> + OK, passed 100 tests.


	// properties for List
	val smax = 50                             //> smax  : Int = 50
	//	- take
	val lTakeProp = forAll(intList) (l => {
			val tl = l.take(smax)
			(tl.size <= smax) &&
			(tl.size <= l.size) &&
			(tl.zip(l).forall(_ match {case (a, b) => a == b}))
		}
	)                                         //> lTakeProp  : fpinscala.testing.Prop = Prop(<function3>)
	run(lTakeProp)                            //> + OK, passed 100 tests.
	//	- drop
	val lDropProp = forAll(intList) (l => {
			val dl = l.drop(smax)
			(if(l.size <= smax) dl.isEmpty else dl.size == (l.size - smax)) &&
			(dl.forall(l.contains(_)))
		}
	)                                         //> lDropProp  : fpinscala.testing.Prop = Prop(<function3>)
	run(lDropProp)                            //> + OK, passed 100 tests.
	
	// properties for Stream
	//	- filter
	val isEvenPred = (i: Int) => i%2 == 0     //> isEvenPred  : Int => Boolean = <function1>
	val isOddPred = (i: Int) => i%2 != 0      //> isOddPred  : Int => Boolean = <function1>
	val sFilterProp = forAll(intStream) (s => {
			val sEven = s filter isEvenPred
			val sOdd = s filter isOddPred
			(sEven forall isEvenPred) &&
			(sOdd forall isOddPred) &&
			(sEven forall (!sOdd.contains(_))) &&
			(sOdd forall (!sEven.contains(_)))
		}
	)                                         //> sFilterProp  : fpinscala.testing.Prop = Prop(<function3>)
	run(sFilterProp)                          //> + OK, passed 100 tests.
	//  - unfold
	//...
	
	// prperties for "sequence" in Option and Either
	val intVal = -41                          //> intVal  : Int = -41
	val errVal = 0                            //> errVal  : Int = 0
	val genOption = Gen.listOf(Gen.boolean map (if (_) Some(intVal) else None))
                                                  //> genOption  : fpinscala.testing.SGen[List[Product with Serializable with fpi
                                                  //| nscala.errorhandling.Option[Int]]] = SGen(<function1>)
	val getEither = Gen.listOf(Gen.boolean map (if (_) Left(errVal) else Right(intVal)))
                                                  //> getEither  : fpinscala.testing.SGen[List[Product with Serializable with fpi
                                                  //| nscala.errorhandling.Either[Int,Int]]] = SGen(<function1>)
	 
	val optSequenceProp = forAll(genOption) (lo => {
			val oSequence = Option.sequence(lo)
			if (lo.isEmpty) oSequence == Some(Nil) else oSequence.getOrElse(Nil).forall(_ == intVal)
		}
	)                                         //> optSequenceProp  : fpinscala.testing.Prop = Prop(<function3>)
	run(optSequenceProp)                      //> + OK, passed 100 tests.
	
	val optEitherProp = forAll(getEither) (eo => {
			val eSequence = Either.sequence(eo)
			if (eo.isEmpty) eSequence == Right(Nil)
			else eSequence match {
				case Left(lval) => lval == errVal
				case Right(rlist) => rlist.forall(_ == intVal)
			}
		}
	)                                         //> optEitherProp  : fpinscala.testing.Prop = Prop(<function3>)
	run(optEitherProp)                        //> + OK, passed 100 tests.
	
}
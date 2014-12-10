package fpinscala.monoids

object MonoidsTestWorksheet {
	val wcstring = "this is the test 12345"   //> wcstring  : String = this is the test 12345
	val cnt = Monoid.count(wcstring)          //> cnt  : Int = 5
	 
  val iseq = IndexedSeq(1,4,7,2,6,8,7,4,7,6,5)    //> iseq  : IndexedSeq[Int] = Vector(1, 4, 7, 2, 6, 8, 7, 4, 7, 6, 5)
  val bg = Monoid.bag(iseq)                       //> bg  : Map[Int,Int] = Map(5 -> 1, 1 -> 1, 6 -> 2, 2 -> 1, 7 -> 3, 8 -> 1, 4 -
                                                  //| > 2)
  val some: Option[Int] = Some(5)                 //> some  : Option[Int] = Some(5)
  val none: Option[Int] = None                    //> none  : Option[Int] = None
  val foldedSome1 = OptionFoldable.foldLeft(some)(3)((b, a) => b + a)
                                                  //> foldedSome1  : Int = 8
  val foldedSome2 = OptionFoldable.foldRight(some)(5)((a, b) => a + b)
                                                  //> foldedSome2  : Int = 10
  val foldedNone1 = OptionFoldable.foldLeft(none)(3)((b, a) => b + a)
                                                  //> foldedNone1  : Int = 3
  val foldedNone2 = OptionFoldable.foldRight(none)(5)((a, b) => a + b)
                                                  //> foldedNone2  : Int = 5
  OptionFoldable.toList(some)                     //> res0: List[Int] = List(5)
  OptionFoldable.toList(none)                     //> res1: List[Int] = List()
  
  import Monoid._
  import fpinscala.testing.Gen
  import fpinscala.state.RNG
  val rng: RNG = RNG.Simple(System.currentTimeMillis)
                                                  //> rng  : fpinscala.state.RNG = Simple(1418190466206)
  
  val prdMTest = monoidLaws[(Int, Int)](productMonoid(intAddition, intMultiplication), Gen.choose(-100, 100).map(i => (i,i)))
                                                  //> prdMTest  : fpinscala.testing.Prop = Prop(<function3>)
  prdMTest.run(100, 100, rng)                     //> res2: fpinscala.testing.Prop.Result = Passed
  
}
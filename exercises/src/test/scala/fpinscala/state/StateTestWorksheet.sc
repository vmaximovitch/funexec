package fpinscala.state

import RNG._

object StateTestWorksheet {
	val seed = -41L                           //> seed  : Long = -41
  val (nextInt, rng1) = RNG.Simple(seed).nextInt  //> nextInt  : Int = -15774705
                                                  //| rng1  : fpinscala.state.RNG = Simple(280441165650070)
  val (nonNegInt, rng2) = RNG.nonNegativeInt(rng1)//> nonNegInt  : Int = 134459596
                                                  //| rng2  : fpinscala.state.RNG = Simple(8811944098793)
  val (dbl1, rng3) = RNG.double(rng2)             //> dbl1  : Double = 0.014097501989454031
                                                  //| rng3  : fpinscala.state.RNG = Simple(279490929678400)
  val (intL, rng4) = RNG.ints(5)(rng3)            //> intL  : List[Int] = List(-1273444460, 712016358, -404121956, -1087500627, 92
                                                  //| 429737)
                                                  //| rng4  : fpinscala.state.RNG = Simple(198018520607231)
  val drnd: Rand[Double] = RNG.double             //> drnd  : fpinscala.state.RNG.Rand[Double] = <function1>
  val (dd1, rng5) = drnd(rng4)                    //> dd1  : Double = 0.5892283315770328
                                                  //| rng5  : fpinscala.state.RNG = Simple(198548461206430)
  val (dd2, rng6) = map2(double, drnd)(_ + _)(rng5)
                                                  //> dd2  : Double = 1.2522659092210233
                                                  //| rng6  : fpinscala.state.RNG = Simple(231380497368712)
  
  val lr = List(double, double, double, double)   //> lr  : List[fpinscala.state.RNG.Rand[Double]] = List(<function1>, <function1>
                                                  //| , <function1>, <function1>)
  val (rl, rng7) = sequence(lr)(rng6)             //> rl  : List[Double] = List(0.5758551261387765, 0.04741232097148895, 0.6982558
                                                  //| 290474117, 0.5406941426917911)
                                                  //| rng7  : fpinscala.state.RNG = Simple(200430572549132)
	val st = State((s: Int) => (s+1, s))      //> st  : fpinscala.state.State[Int,Int] = State(<function1>)
	st.map(_ * 3).run(1)                      //> res0: (Int, Int) = (6,1)
	State.get.run(2)                          //> res1: (Int, Int) = (2,2)
	
	State.simulateMachine(List(Coin, Coin, Turn, Turn)).run(Machine(false, 3, 2))
                                                  //> res2: ((Int, Int), fpinscala.state.Machine) = ((3,2),Machine(false,2,3))
}
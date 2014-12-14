package fpinscala.monads

object MonadTestWorksheet {
  val testList = List (1, 2, 3, 4, 5, 6 , 7)      //> testList  : List[Int] = List(1, 2, 3, 4, 5, 6, 7)
  val mfe = Monad.optionMonad.filterM(testList)(a => Some(a % 2 == 0))
                                                  //> mfe  : Option[List[Int]] = Some(List(2, 4, 6))
  val mfo = Monad.optionMonad.filterM(testList)(a => Some(a % 2 != 0))
                                                  //> mfo  : Option[List[Int]] = Some(List(1, 3, 5, 7))
  val mfn = Monad.optionMonad.filterM(testList)(a => None)
                                                  //> mfn  : Option[List[Int]] = None

}
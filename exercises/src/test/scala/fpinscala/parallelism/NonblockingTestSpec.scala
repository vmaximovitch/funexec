/**
 */
package fpinscala.parallelism

import org.scalatest._
import Nonblocking._
import java.util.concurrent._

/** @author vladimirmaksimovich
 *
 */
class NonblockingTestSpec extends FlatSpec with Matchers {
  def fixture = new {
    val executor: ExecutorService = Executors.newFixedThreadPool(10)
    val condForkT = Par.fork(Par.unit(true))
    val condForkF = Par.fork(Par.unit(false))
    val parFork1 = Par.fork(Par.unit(1))
    val parFork2 = Par.fork(Par.unit(2))
    
    def toString[?](p: Par.Par[?]): String =
      Par.run(executor)(p).get.toString
  }

  "Nonblocking.choice" should "return one of 2 choices" in {
    val f = fixture
    val parChoiceT = Par.choice(f.condForkT)(f.parFork1, f.parFork2)
    val parChoiceF = Par.choice(f.condForkF)(f.parFork1, f.parFork2)
    val parChoiceViaNT = Par.choice(f.condForkT)(f.parFork1, f.parFork2)
    val parChoiceViaNF = Par.choice(f.condForkF)(f.parFork1, f.parFork2)
    info("choice with " + f.toString(f.condForkT) + " for " + f.toString(f.parFork1) +
        " or " + f.toString(f.parFork2) + " is " + f.toString(parChoiceT))
    Par.run(f.executor)(parChoiceT).get shouldBe 1
    Par.run(f.executor)(parChoiceViaNT).get shouldBe 1
    info("choice with " + f.toString(f.condForkF) + " for " + f.toString(f.parFork1) +
        " or " + f.toString(f.parFork2) + " is " + f.toString(parChoiceF))
    Par.run(f.executor)(parChoiceF).get shouldBe 2
    Par.run(f.executor)(parChoiceViaNF).get shouldBe 2
  }
  
  

}
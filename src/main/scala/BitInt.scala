//Big Int example
//
import java.math.BigInteger

class factorial{
  //use scala class
  def factorial0(x : BigInt): BigInt =
    if (x == 0) 1 else x * factorial0(x - 1)
  
  //use java class
  def factorial1(x : BigInteger): BigInteger =
    if (x == BigInteger.ZERO) BigInteger.ONE else x.multiply(factorial1(x.subtract(BigInteger.ONE)))
}

object factorial_test {
  def main(args: Array[String]) {
    val factorial_inst = new factorial
    println("Use Scala Class:"+factorial_inst.factorial0(30))
    println("Use Java Class:"+factorial_inst.factorial1(BigInteger.valueOf(30)))
    println("KK")
  }
}
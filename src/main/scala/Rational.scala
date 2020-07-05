class Rational (n:Int, d:Int) {
  //添加前置条件,前置条件不满足抛出IllegalArgumentException
  require(d != 0)
  //最大公约数计算
  private val g = gcd(n.abs,d.abs)
  //将类参做成字段
  val number:Int=n/g
  val denom :Int=d/g
  //重写toString方法
  override def toString: String = number + "/" + denom
  //添加+,*方法,scala中*操作符优先级比+高
  //加入方法重载
  def + (that: Rational): Rational = new Rational(
    number * that.denom + that.number * denom,
    denom * that.denom
  )
  def + (i:Int): Rational = new Rational(number+i*denom,denom)

  def - (that: Rational): Rational = new Rational(
    number * that.denom - that.number * denom,
    denom * that.denom
  )
  def - (i:Int): Rational = new Rational(number-i*denom,denom)

  def * (that: Rational): Rational = new Rational(number * that.number, denom * that.denom)
  def * (i:Int): Rational = new Rational(number * i, denom)

  def / (that: Rational): Rational = new Rational(number * that.denom , denom * that.number)
  def / (i:Int): Rational = new Rational(number, denom * i)

  //添加lessthan方法,this执行lessThan调用的对象
  def lessThan(that: Rational): Boolean =
    this.number * that.denom < that.number * this.denom
  //基于lessThan添加max方法
  def max(that: Rational): Rational =
    if(lessThan(that)) that else this
  //辅助构造方法，使用于分母为1的情况，只需要传入一个参数即可
  def this(n:Int) = this(n,1)
  //def this(n:Int,d:Int) = this(n,d),不能定义成和主构造方法参数一样！！
  //使用私有修饰符，寻找最大公约数
  private def gcd(a:Int,b:Int): Int = if(b==0) a else gcd(b,a%b)
}

//val rational = new Rational(3,2)
object Rational{
  def main(args: Array[String]): Unit = {
    val a = new Rational(3,2)
    val b = new Rational(3,6)
    val c = a + b * a
    val d = new Rational(5)
    println(a)
    println(b)
    println(c)
    println(a lessThan b)
    println(a max b)
    println(d)
    println(c*2)
    implicit def intToRational(x:Int):Rational = new Rational(x)
    println(2*c)
  }
}

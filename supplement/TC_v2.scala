package version1

/* Example 1: abstract ordering library */
trait Ord[A]:
  extension (a:A)
    def cmp(b:A): Int
    def ===(b:A) = a.cmp(b) == 0
    def < (b:A) = a.cmp(b) < 0
    def > (b:A) = a.cmp(b) > 0
    def <= (b:A) = a.cmp(b) <= 0
    def >= (b:A) = a.cmp(b) >= 0

def max3[A: Ord](a: A, b: A, c: A) : A =
  if (a<=b) {if (b<=c) c else b }
  else {if (a<=c) c else a }

given intOrd : Ord[Int] with
  extension (a: Int)
    def cmp(b: Int) = a - b

/* Example 1-1: abstract ordered list */
class Bag[A: Ord] protected (val toList: List[A]) { 
  def this() = this(Nil)
  def add(x: A) : Bag[A] = {
    def loop(elmts: List[A]) : List[A] =
      elmts match {
        case Nil => x :: Nil
        case e :: _ if (x < e) => x :: elmts
        case e :: _ if (x === e) => elmts
        case e :: rest => e :: loop(rest)
      }
    new Bag(loop(toList))
  }
}

/* Example 1-2: implementations, etc. */
given tupOrd[A, B](using Ord[A], Ord[B]): Ord[(A,B)] with
  extension (a: (A,B))
    def cmp(b: (A, B)) : Int = {
      val c1 = a._1.cmp(b._1)
      if (c1 != 0) c1
      else { a._2.cmp(b._2) }
    }

def intOrdRev : Ord[Int] = new {
  extension (a: Int)
    def cmp(b: Int) = b - a
}

/* Example 2: iterator */
trait Iter[I,A]:
  extension (i: I)
    def getValue: Option[A]
    def getNext: I

trait Iterable[R,A]:
  type Itr
  given ItrI: Iter[Itr,A]
  extension (r: R)
    def iter: Itr

given iter2iterable[I,A](using iterI: Iter[I,A]): Iterable[I,A] with
  type Itr = I
  def ItrI = iterI
  extension (i:I)
    def iter = i

def sumElements[I](xs: I)(implicit II:Iterable[I,Int]) = {
  def loop(i: II.Itr): Int =
    i.getValue match {
      case None => 0
      case Some(n) => n + loop(i.getNext)
    }
  loop(xs.iter)
}

def printElements[I,A](xs: I)(implicit II: Iterable[I,A]) = {
  def loop(i: II.Itr): Unit =
    i.getValue match {
      case None =>
      case Some(a) => {println(a); loop(i.getNext)}
  }
  loop(xs.iter)
}

/* Example 2-1: list-like and tree-like type */
/* introduction is implemented with method of unit */
trait Listlike[L,A]:
  extension(u:Unit)
    def unary_! : L
  extension(elem:A)
    def ::(l: L): L
  extension(l: L)
    def head: Option[A]
    def tail: L
    def ++(l2: L): L

trait Treelike[T,A]:
  extension(u:Unit)
    def unary_! : T
  extension(a:A)
    def node(lt: T, rt: T): T
  extension(t: T)
    def root : Option[A]
    def left : T
    def right : T

def testList[L](implicit ListI: Listlike[L,Int], ItrI: Iterable[L,Int]) = {
  val l = (3 :: !()) ++ (1 :: 2 :: !())
  println(sumElements(l)) 
  printElements(l)
}

def testTree[T](implicit TreeI: Treelike[T,Int], ItrI: Iterable[T,Int]) = {
  val t = 3.node(4.node(!(), !()), 2.node(!(),!()))
  println(sumElements(t))
  printElements(t)
}

/* Example 2-2: implementation of interfaces in the case of list */
given listIter[A]: Iter[List[A],A] with
  extension (l: List[A])
    def getValue = l.headOption
    def getNext = l.tail

given listListlike[A]: Listlike[List[A],A] with
  extension (u: Unit)
    def unary_! = Nil
  extension (a: A)
    def ::(l: List[A]) = a::l
  extension (l: List[A])
    def head = l.headOption
    def tail = l.tail
    def ++(l2: List[A]) = l ::: l2

/* Example 2-3: implementation of interfaces in the case of tree */
enum MyTree[+A]:
  case Leaf
  case Node(value: A, left: MyTree[A], right: MyTree[A])
import MyTree._

/* iterator of tree has list-like internal iterator */
given treeIterable[L,A](using listI: Listlike[L,A], iterI: Iter[L,A]) : Iterable[MyTree[A], A] with
  type Itr = L
  def ItrI = iterI
  extension (t: MyTree[A])
    def iter: L =
      t match {
        case Leaf => !()
        case Node(v, lt, rt) => v :: (lt.iter ++ rt.iter)
      }

given mytreeTreelike[A] : Treelike[MyTree[A],A] with
  extension (u: Unit)
    def unary_! = Leaf
  extension (a: A)
    def node(l: MyTree[A], r: MyTree[A]) = Node(a,l,r)
  extension (t: MyTree[A])
    def root = 
      t match {
        case Leaf => None
        case Node(v,_,_) => Some(v)
      }
    def left =
      t match {
        case Leaf => t
        case Node(_,lt,_) => lt
      }
    def right = 
      t match {
        case Leaf => t
        case Node(_,_,rt) => rt
      }

/* Example 3: stack */
trait Stack[S,A]:
  extension (u: Unit)
    def empty : S
  extension (s: S)
    def get: (A,S)
    def put(a: A): S

def testStack[S](implicit StkI: Stack[S,Int]) = {
  val s0 = ().empty
  val s1 = s0.put(3)
  val s2 = s1.put(-2)
  val s3 = s2.put(4)
  val (v1,s4) = s3.get
  val (v2,s5) = s4.get
  (v1,v2)
}

given BasicStack[A] : Stack[List[A],A] with
  extension (u: Unit)
    def empty = List()
  extension (s: List[A])
    def get = (s.head, s.tail)
    def put(a: A) = a :: s

def StackOverridePut[S,A](newPut: (S,A)=>S)(implicit stkI: Stack[S,A]) =
  new Stack[S,A] {
    extension (u: Unit)
      def empty = stkI.empty(u)
    extension (s: S)
      def get = stkI.get(s)
      def put(a: A) = newPut(s,a)
  }

def Doubling[S](implicit stkI: Stack[S,Int]) : Stack[S,Int] =
  StackOverridePut((s,a) => s.put(2 * a))
def Incrementing[S](implicit stkI: Stack[S,Int]) : Stack[S,Int] =
  StackOverridePut((s,a) => s.put(a + 1))
def Filtering[S](implicit stkI: Stack[S,Int]) : Stack[S,Int] =
  StackOverridePut((s,a) => if (a >= 0) s.put(a) else s)

def SortedStack : Stack[List[Int],Int] = new {
  extension (u: Unit)
    def empty = List()
  extension (s: List[Int])
    def get = (s.head, s.tail)
    def put(a: Int) : List[Int] = {
      def loop(l: List[Int]) : List[Int] =
        l match {
          case Nil => a :: Nil
          case hd :: tl => if (a <= hd) a :: l else hd :: loop(tl)
        }
      loop(s)
    }
}

/* Example 3 : Box */
import scala.language.implicitConversions // needed for implicit conversion of D into Box[S]

// S -> S with data
trait Box[S[_]]:
  type Data
  val * : Data
  given DI: S[Data]

object Box {
  implicit def apply[S[_],D](d: D)(implicit i: S[D]): Box[S] =
    new Box[S] {
      type Data = D
      val * = d
      val DI = i
    }
}

trait DataProcessor[D]:
  extension (d: D)
    def input(s: String): D
    def output: String

trait DPFactory:
  extension (u: Unit)
    def getTypes: List[String]
    def makeDP(dptype: String): Box[DataProcessor]

trait UserInteraction:
  def run(implicit factory: DPFactory): Unit

val userInteraction: UserInteraction = new UserInteraction {
  def run(implicit factory: DPFactory) = {
    val dptype = scala.io.StdIn.readLine("Input a processor type " + ().getTypes.toString + ": ")
    val d = ().makeDP(dptype)
    val d_done = getInputs(d)
    printOutputs(d_done.*)
  }
  def getInputs(d: Box[DataProcessor]): Box[DataProcessor] = {
    val d2 = d.*.input(scala.io.StdIn.readLine("Input Data: "))
    val done = scala.io.StdIn.readLine("More inputs? [Y/N]: ")
    if (done.toLowerCase() == "n") d2
    else getInputs(d2)
  }
  def printOutputs[D](d: D)(implicit DP: DataProcessor[D]) = {
    println("The result of processing your inputs is:")
    println(d.output)
  }
}

given dpfactory: DPFactory with
  extension (u: Unit)
    def getTypes = List("sum", "mult")

    def makeDP(dptype: String) = {
      if (dptype == "sum")
        makeProc(0, (x, y) => x + y)
      else
        makeProc(1, (x, y) => x * y)
    }

    def makeProc(init: Int, op: (Int, Int) => Int): Box[DataProcessor] = {
      given dp: DataProcessor[Int] with
        extension (d: Int)
          def input(s: String) = op(d, s.toInt)
          def output = d.toString()

      init
    }

//   userInteraction.run(dpfactory)
// testStack(Filtering(Incrementing(Doubling(SortedStack))))
// testStack
// // testStack(Filtering(Incrementing (Doubling(BasicStack))))
// testStack(Filtering(Incrementing (Doubling)))
// // testStack(Filtering(Incrementing(Incrementing(Doubling(BasicStack)))))
// testStack(Filtering(Incrementing(Incrementing(Doubling))))
// testStack(Filtering(Incrementing(Doubling(SortedStack))))
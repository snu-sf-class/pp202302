package version2

/* Example 1: abstract ordering library */
trait Ord[A] {
  def cmp(me: A, you: A): Int
  def ===(me: A, you: A): Boolean = cmp(me,you) == 0
  def < (me: A, you: A): Boolean = cmp(me,you) < 0
  def > (me: A, you: A): Boolean = cmp(me,you) > 0
  def <= (me: A, you: A): Boolean = cmp(me,you) <= 0
  def >= (me: A, you: A): Boolean = cmp(me,you) >= 0
}

def max3[A](a: A, b: A, c: A)(implicit ord: Ord[A]) : A =
  if (ord.<=(a, b)) { if (ord.<=(b,c)) c else b }
  else { if (ord.<=(a,c)) c else a }

implicit val intOrd : Ord[Int] = new Ord[Int] {
    def cmp(me: Int, you: Int) = me - you 
}

/* Example 1-1: abstract ordered list */
class Bag[A] protected (val toList: List[A])(implicit ord: Ord[A]) { 
    def this()(implicit ord: Ord[A]) = this(Nil)(ord)
    def add(x: A) : Bag[A] = {
        def go(elmts: List[A]) : List[A] =
            elmts match {
                case Nil => x :: Nil
                case e :: _ if (ord.<(x,e)) => x :: elmts
                case e :: _ if (ord.===(x,e)) => elmts
                case e :: rest => e :: go(rest)
            }
        new Bag(go(toList))
    }
}

/* Example 1-2: implementations, etc. */
implicit def tupOrd[A, B](implicit ordA: Ord[A], ordB: Ord[B]) 
    : Ord[(A, B)] =
    new Ord[(A, B)] {
        def cmp(me: (A, B), you: (A, B)) : Int = { 
            val c1 = ordA.cmp(me._1, you._1)
            if (c1 != 0) c1
            else { ordB.cmp(me._2, you._2) }
        }
    }

val intOrdRev : Ord[Int] = new Ord[Int] { def cmp(me: Int, you: Int) = you - me }


/* Example 2: iterator */
trait Iter[I,A] {
  def getValue(i: I): Option[A]
  def getNext(i: I): I
}

trait Iterable[R,A] {
  type Itr
  def iterIF: Iter[Itr, A]
  def iter(a: R): Itr
}

implicit def iter2iterable[I,A](implicit iterI: Iter[I,A]): Iterable[I,A] =
  new Iterable[I,A] {
    type Itr = I
    def iterIF = iterI
    def iter(i:I) = i
  }

def sumElements[R](xs: R)(implicit II:Iterable[R,Int]) = {
  def loop(i: II.Itr): Int =
    II.iterIF.getValue(i) match {
      case None => 0
      case Some(n) => n + loop(II.iterIF.getNext(i))
    }
  loop(II.iter(xs))
}

def printElements[R,A](xs: R)(implicit II: Iterable[R,A]) = {
  def loop(i: II.Itr): Unit =
    II.iterIF.getValue(i) match {
      case None =>
      case Some(a) => {println(a); loop(II.iterIF.getNext(i))}
  }
  loop(II.iter(xs))
}

/* this is the scala2 implementation of sumElements % printElements */
def _sumElements[R](xs: R)(implicit IT: Iter[R,Int]) : Int =
  IT.getValue(xs) match {
    case None => 0
    case Some(n) => n + _sumElements(IT.getNext(xs))
  }
  
def _printElements[R,A](xs: R)(implicit IT: Iter[R,A]) : Unit =
  IT.getValue(xs) match {
    case None =>
    case Some(n) => { println(n); _printElements(IT.getNext(xs))}
  }

def sumElements2[R](xs: R)(implicit ITR: Iterable[R,Int]) =
  _sumElements(ITR.iter(xs))(ITR.iterIF)

def printElements2[R,A](xs: R)(implicit ITR: Iterable[R,A]) =
  _printElements(ITR.iter(xs))(ITR.iterIF)

/* Example 2-1: list-like and tree-like type */
/* introduction is implemented with method of unit */
trait Listlike[L,A] {
  def empty : L
  def head(l: L) : Option[A]
  def tail(l: L) : L
  def cons(a: A, l: L) : L
  def append(l1: L, l2: L) : L
}

trait Treelike[T,A] {
  def empty : T
  def node(a: A, l: T, r: T) : T
  def head(t: T) : Option[A]
  def left(t: T) : T
  def right(t: T) : T
}

def testList[L](implicit LI: Listlike[L,Int], IT: Iter[L,Int]) = {
  val l = LI.append(LI.cons(3, LI.empty), LI.cons(1, LI.cons(1, LI.empty)))
  /* Question: Is this term type-checked well? why? */
  println(sumElements(l))
  printElements(l)
}

def testTree[T](implicit TI: Treelike[T,Int], ITR: Iterable[T,Int]) = {
  val t: T = TI.node(3, TI.node(4, TI.empty, TI.empty), TI.node(2, TI.empty, TI.empty))
  println(sumElements2(t))
  printElements2(t)
}

/* Example 2-2: implementation of interfaces in the case of list */
implicit def listIter[A] : Iter[List[A], A] =
  new Iter[List[A],A] {
    def getValue(a: List[A]) = a.headOption
    def getNext(a: List[A]) = a.tail
  }

implicit def listListlike[A] : Listlike[List[A],A] =
  new Listlike[List[A],A] {
    def empty: List[A] = Nil
    def head(l: List[A]) = l.headOption
    def tail(l: List[A]) = l.tail
    def cons(a: A, l: List[A]) = a :: l
    def append(l1: List[A], l2: List[A]) = l1 ::: l2
  }

/* Example 2-3: implementation of interfaces in the case of tree */
enum MyTree[+A]:
  case Leaf
  case Node(value: A, left: MyTree[A], right: MyTree[A])
import MyTree._

def treeIterable[L,A](implicit IF: Listlike[L,A], IT: Iter[L,A]) : Iterable[MyTree[A], A] =
  new Iterable[MyTree[A], A] {
    type Itr = L
    def iter(a: MyTree[A]): L =
      a match {
        case Leaf => IF.empty
        case Node(v, left, right) => IF.cons (v, IF.append(iter(left), iter(right)))
      }
    val iterIF = IT
  }

implicit def treeIterableList[A] : Iterable[MyTree[A], A] = treeIterable[List[A],A]

implicit def mytreeTreelike[A] : Treelike[MyTree[A],A] =
  new Treelike[MyTree[A],A] {
    def empty = Leaf
    def node(a: A, l: MyTree[A], r: MyTree[A]) = Node(a,l,r)
    def head(t: MyTree[A]) =
      t match {
        case Leaf => None
        case Node(v,_,_) => Some(v) 
      }
    def left(t: MyTree[A]) =
      t match {
        case Leaf => t
        case Node(_,lt,_) => lt
      }
    def right(t: MyTree[A]) =
      t match {
        case Leaf => t
        case Node(_,_,rt) => rt
      }
  }

/* Example 3 : Box */
trait Box[S[_]]:
  type Data
  val * : Data
  def DI: S[Data]

object Box {
  implicit def apply[S[_],D](d: D)(implicit i: S[D]): Box[S] =
    new Box[S] {
      type Data = D
      val * = d
      val DI = i
    }
}

trait DataProcessor[D]:
  def input(d: D, s: String): D
  def output(d: D): String

trait DPFactory:
  def getTypes: List[String]
  def makeDP(dptype: String): Box[DataProcessor]

trait UserInteraction:
  def run(implicit factory: DPFactory): Unit

val userInteraction: UserInteraction = new UserInteraction {
  def run(implicit factory: DPFactory) = {
    val dptype = scala.io.StdIn.readLine("Input a processor type " + factory.getTypes.toString + ": ")
    val d = factory.makeDP(dptype)
    val d_done = getInputs(d)
    printOutputs(d_done.*)(d_done.DI)
  }
  def getInputs(d: Box[DataProcessor]): Box[DataProcessor] = {
    val d2 = d.DI.input(d.*, scala.io.StdIn.readLine("Input Data: "))
    val done = scala.io.StdIn.readLine("More inputs? [Y/N]: ")
    if (done.toLowerCase() == "n") Box.apply(d2)(d.DI)
    else getInputs(Box.apply(d2)(d.DI))
  }
  def printOutputs[D](d: D)(implicit DP: DataProcessor[D]) = {
    println("The result of processing your inputs is:")
    println(DP.output(d))
  }
}

implicit def dpfactory: DPFactory = 
  new DPFactory {
    def getTypes = List("sum", "mult")

    def makeDP(dptype: String) = {
      if (dptype == "sum")
        makeProc(0, (x, y) => x + y)
      else
        makeProc(1, (x, y) => x * y)
    }
    def makeProc(init: Int, op: (Int, Int) => Int): Box[DataProcessor] = 
      new Box {
        type Data = Int
        val * = init
        def DI: DataProcessor[Int] = new DataProcessor[Int] {
          def input(d: Int, s: String) = op(d, s.toInt)
          def output(d: Int) = d.toString()
        }
      }

  }
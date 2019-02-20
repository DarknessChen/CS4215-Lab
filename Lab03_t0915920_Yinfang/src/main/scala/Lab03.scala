import scala.annotation.tailrec

object Lab03 {

  def main(args: Array[String]): Unit = {
    /*
     * There are four main parts for you to practice with:
     *     Lists, Numbers, Trees and Higher Order functions
    */
    testCasesLists ()
    testCasesNumbers ()
    testCasesTrees ()
    testCasesHigherOrder ()
  }

  def testCasesLists ():Unit = {
    // There are 6 test cases for Lists exercises
    println ("01): " + last_snd  ( ls1)) // Some(c)
    //println ("01): " + last_snd  ( List ())) // None
    println ("02): " + compress  ( ls2)) // List(a, b, c, a, d, e)
    println ("03): " + removeDupl( ls2)) // List(a, b, c, d, e)
    println ("04): " + findFirst (((x:Int)  => x % 2 == 0), List(3,6,7,3,4,8,3,3,3)))// Some(6)
    println ("05): " + findLast  (((x:Int)  => x % 2 == 0), List(3,6,7,3,4,8,3,3,3)))// Some(8)
    println ("06): " + genPairs  (6) ) // List((1,5), (2,4), (3,3), (4,2), (5,1))
  }
  def testCasesNumbers ():Unit = {
    // There are 5 test cases for Numbers exercises
    println ("07): " + isPrime   (13))  // true
    println ("08): " + allPrimes (1, 100)) //List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)
    println ("09): " + pfactors  (315))  // List(3, 3, 5, 7)
    println ("10): " + pfactorsM (315)) // List((3,2), (5,1), (7,1))
    println ("11): " + goldbach  (28)) // (5, 23)
  }

  def testCasesTrees ():Unit = {
    // There are 4 test cases for Trees exercises
    println ("12): " + countL   ( rt1) ) // 3
    println ("13): " + prefixBT ( rt1) ) // List(1, 3, 4, 5, 2)
    println ("14): " + infixBT  ( rt1) ) // List(4, 3, 5, 1, 2)
    println ("15): " + perfectTree (3) ) //Node(1,Node(1,Leaf(1),Leaf(1)),Node(1,Leaf(1),Leaf(1)))
  }

  def testCasesHigherOrder ():Unit = {
    // There are 5 test cases for higher order functions
    println ("16): " + prod (List(1,2), List('a','b','c'))) // List((1,a), (1,b), (1,c), (2,a), (2,b), (2,c))
    println ("17): " + prefixRT    ( rt2))  // List(1, 2, 3, 4, 5)
    println ("18): " + prefixRTHO  ( rt2))  // List(1, 2, 3, 4, 5)
    println ("19): " + postfixRTHO ( rt2))  // List(2, 4, 3, 5, 1)
    println ("20): " + string_of_RT( rt2))  // 1(2,3(4),5)
  }

  val ls1 = List('a' , 'b' , 'c' , 'd' )
  val ls2 = List('a','a','a','a','b','c','c','a','a','d','e','e','e','e')

  def last_snd [A] (xs: List[A]) : Option [A] = {
    /* # 01
     * Implement a function that would return the
     * 2nd last element. If only one element exist,
     * return that element. For example:
     * 	last_two (List (1,2,3,4,5)) ===> Some (4)
     * 	last_two (List (5)) ===> Some (5)
     * 	last_two (List ()) ===> None
    */

//    xs match {
//      case Nil => None
//      case x::Nil => Some(x)
//      case _ => Some(xs(xs.length-2))
//    }

    if(xs.isEmpty){
      None
    } else {
      //Some(xs.init.last)
      Some(xs.takeRight(2).head)
    }
  }

  def compress [A] (xs: List[A]) : List [A] = xs match {
    /* # 02
       * Implement a recursive function that would remove
       * duplicates that occur consecutively.
       * For example:
       * 	compress (List(1,1,2,2,1)) ==> List(1,2,1)
       */
    case Nil => Nil
    case h::List() => List(h)
    case h::tail if (h == tail.head) => compress(tail)
    case h::tail => h::compress(tail)
  }

  def removeDupl [A] (xs: List[A]) : List [A] = {
    /* # 03
     * Implement a function that would remove
     * all duplicates in a list.
     * For example:
     * 	 removeDupl (List(1,1,2,2,1)) ==> List(1,2)
     */

    // First way is to use distinct
    xs.distinct

    // Second method is to use fold high order function
//    xs.foldRight(List[A]()) ((x:A, acc:List[A]) =>
//      if(acc exists (_ == x)){    // if(rest exists (_ == x))
//        acc
//      } else {
//        x :: acc
//      })
  }

  def findFirst[A] (fx: A => Boolean, xs:List[A]): Option [A] = {
    /* # 04
     * Implement a function that would return the
     * first element in a list that satisfies a given predicate
     * For example:
     * 	 findFirst (((x:Int)  => x > 1), List(1,1,2,1,4,1))  ==> Some (2)
     * 	 findFirst (((x:Int)  => x > 4), List(1,1,2,1,4,1))) ==> None
     */

    xs.foldLeft[Option[A]] (None)((acc: Option[A], x: A) =>
      acc match {
        case Some(_) => acc
        case None    =>
          if (fx(x)) Some(x)
          else None
      })
  }

  def findLast[A] (fx: A => Boolean, xs:List[A]): Option [A] = {
    /* # 05
     * Implement a function that would return the
     * last element in a list that satisfies a given predicate
     * For example:
     * 	 findFirst (((x:Int)  => x > 1), List(1,1,2,1,4,1))  ==> Some (4)
     * 	 findFirst (((x:Int)  => x > 4), List(1,1,2,1,4,1))) ==> None
     */
    xs.foldRight [Option[A]] (None) ((x: A, acc: Option[A]) =>
      acc match {
        case Some(_) => acc
        case None    =>
          if (fx(x)) Some(x)
          else None
      })
  }

  def genPairs (num:Int):List [(Int, Int)] = {
    /* # 06
     * Given a number n>1, generate all possible
     * pairs of positive numbers (a,b) such that n=a+b
     * For example:
     * 		genPairs (3) ===> List((1,2), (2,1))
     */
    val res = for (i <- 1 until num)
              yield (i, num-i)
    res.toList
  }

  def isPrime(num:Int):Boolean = {
    /* # 07
     * Given a number n, return true if it is a prime number
     * otherwise return false
     * For example:
     * 		isPrime (2) ==> true
     * 		isPrime (4) ==> false
     */
    val max_d = scala.math.sqrt(num).toInt
//    (num>1) && ((2 to max_d) forall (num % _ != 0))
    (num>1) && !((2 to max_d) exists (x => num % x == 0))
  }

  def allPrimes(start:Int, end:Int):List [Int] = {
    /* # 08
     * Given a range of integers by its lower and upper limit,
     * construct a list of all prime numbers in that range.
     * For example:
     * 		allPrimes (10, 2) ==> List()
     * 		allPrimes (2, 10) ==> List(2, 3, 5, 7)
     */
    val list = (start to end)
    val temp = list.filter((p:Int) => isPrime(p)) // val temp = list.filter((isPrime(_))
    temp.toList
  }

  def pfactors (num: Int):List [Int] = {
    /* # 09
     * Given a number, return its prime factors.
     * For example:
     * 		pfactors (6)  ==> List(2,3))
     * 		pfactors (12) ==> List(2,2,3))
     */
     var result = List[Int]()
//     val testList = allPrimes(2, num)
//     val x = testList.head
     var i = num
     for(x <- 2 to num) {
       while((i % x)==0 && isPrime(x)) {
         result = x::result
         i = i / x
       }
     }
     result.reverse
  }

  def pfactorsM (num: Int):List [(Int, Int)] = {
    /* # 10
     * Given a number, return a list of tuples, representing
     * unique prime factors and their occurrences.
     * For example:
     * 		pfactorsM 6  ==> List((2,1), (3,1))
     * 		pfactorsM 12 ==> List((2,2), (3,1))
     */

    var result = List[(Int, Int)]()
    //     val testList = allPrimes(2, num)
    //     val x = testList.head
    var i = num
    var acc = 0
    for(x <- 2 to num) {
      while((i % x)==0 && isPrime(x)) {
        i = i / x
        acc += 1
      }
      if (acc != 0) result = (x, acc)::result
      acc = 0
    }
    result.reverse
  }

  def goldbach (num : Int) : (Int, Int) = {
    /* # 11
     * Goldbach's conjecture says that every positive even number greater
     * than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is
     * one of the most famous facts in number theory that has not been proved
     * to be correct in the general case. It has been numerically confirmed
     * up to very large numbers. Write a function to find the two prime
     * numbers that sum up to a given even integer.
     * For example:
     * 		goldbach 4 ==> (2,2)
     * 		goldbach 8 ==> (3,5)
     */

    val allPair = genPairs(num)
    var result = (0, 0)   // How to avoid using var and use the value inside the loop
    for(tuple <- allPair){
      if(isPrime(tuple._1) && isPrime(tuple._2) && (tuple._1 + tuple._2 == num)){
        result = (tuple._2, tuple._1)
      }
    }
    result
  }

  // Here is the definition of Trees
  sealed trait Tree[A]  // sealed means that it can be extended only in this file
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
  // case class has the parameters -> useful for pattern matching

  val rt1 = Node (1,(Node (3,(Leaf (4)),(Leaf( 5)))), Leaf (2))


  def countL[A] (tree: Tree[A]) : Int = {

    /* # 12
     * Write a function that would count the number of leaves
     * in a given binary tree
     * For example:
     * 		countL (Leaf (0)) ==> 1
     * 		countL (Node(0,(Leaf (0)),Node(0,Leaf( 0),Leaf (0)))) ==> 3
     */

    tree match {
      case Leaf(_) => 1
      case Node (_, l, r) => countL(l) + countL(r)
    }
  }

  def prefixBT [A] (tree: Tree[A]):List [A]= {
    /* # 13
     * We can flatten a tree into a list in prefix fashion
     * by putting value at node, then values of left subtreee,
     * followed by values of right subtrees.
     * For example:
     * 		prefixBT (Node(4,Leaf (1), Leaf (2))) ==> List(4, 1, 2)
     */
    tree match {
      case Leaf(v) => List(v)
      case Node(v, l, r) => List(v) ++ prefixBT(l) ++ prefixBT(r)
    }
  }

  def infixBT [A] (tree: Tree[A]):List [A]= {
    /* # 14
     * We can flatten a tree into a list in infix fashion
     * by putting values of left subtreee, value at node,
     * followed by values of right subtrees.
     * For example:
     * 		infixBT (Node(4,Leaf (1), Leaf (2))) ==> List(1, 4, 2)
     */
    tree match {
      case Leaf(v) => List(v)
      case Node(v, l, r) => infixBT(l) ++ List(v) ++infixBT(r)
    }
  }

  def perfectTree (num : Int) : Tree [Int] = {
    /* # 15
     * A tree is perfectly balanced if either it is a leaf
     * or it is a node with two subtrees of the same height and also
     * perfectly balanced. Write a function that takes a height
     * value and then returning a perfect tree of that height with
     * all its elements set to 1
     * For example:
     * 		perfectTree (2) ==> Node(1,Leaf(1),Leaf(1))
     *
     */
    num match {
      case 1 => Leaf(1)
      case _ => Node(1, perfectTree(num-1), perfectTree(num-1))
    }
  }

  def prod [A, B] (xs: List [A], ys:List [B]): List [(A,B)] = {
    /* # 16
     * Given two lists, return a list of all possible
     * pairs of the two lists.
     * For example
     * 		  prod (List(1,2), List('a','b')) ===>
     * 				List((1,a), (1,b), (2,a), (2,b))
     * Use higher-order function 'map' to help you in this task.
     */
//    for(x <- xs; y <- ys) yield (x, y)

    val v = xs map (x => ys map (y => (x, y)))
    v.flatten
  }


  // polymorphic rose tree
  sealed trait roseTree[A]
  case class NodeR[A] (value:A, list: List[roseTree[A]]) extends roseTree[A]

  val rt2 = NodeR (1, List ( NodeR (2,List()),NodeR (3,List(NodeR (4,List()))),NodeR (5,List())))

  /* # 17
   * We can flatten a rosetree into a list in prefix fashion
   * by putting value at node, followed by values of each
   * of the subtrees.
   * Implement a first-order version of this prefixRT
   * method without using any higher-order functions.
   * For example:
   * 		prefixRT (NodeR(4,List(NodeR (1,List()), NodeR (2,List()))))  ===>
   * 		List(4, 1, 2)
   * Below is a first-order implementation.
   */
  def prefixRT [A] (xs :roseTree[A] ) : List[A] = xs match{
    case NodeR(v,res) => List (v) ::: comb_prefixRT (res) // ::: -> ++
  }

  def comb_prefixRT [A] (xs : List[roseTree[A]] ) : List[A] = xs match{
    case List() => List ()
    case _ => prefixRT (xs.head) ::: comb_prefixRT (xs.tail)
  }

  def prefixRTHO [A] (xs :roseTree[A] ) : List[A] = {
    /* # 18
     * write a higher-order counterpart for prefixRT
     * Use higher-order function "foldRight" to help
     * you in this method.
     */

    xs match {
      case NodeR(v, res) =>
        v :: res.foldRight(List[A]()) ((x:roseTree[A], acc:List[A]) =>
          prefixRTHO(x) ::: acc
        )
    }
  }
  def postfixRTHO [A] (xs :roseTree[A] ) : List[A] = {
    /* # 19
     * write a higher-order counterpart for postfixRT
     * Use higher-order function 'foldRight' to help
     * you in this method.
     */

    val lt = List(1, 3, 6, 10)
    lt.reduce((total, cur) => total + cur)

    def op(total: Int, cur: Int) = total + cur
    lt reduce op

    lt.foldLeft(0)(op)






    xs match {

      case NodeR(v, res) =>
        res.foldRight(List[A]()) ((x:roseTree[A], acc:List[A]) =>
          postfixRTHO(x) ::: acc
        ) :+ v
    }
  }

  def string_of_RT[A] ( xs :roseTree[A] ) : String =  {
    /* # 20
     * Write a function which generates such a string representation
     * for rose tree.
     * which prints a list of items separated by comma.
     */
    xs match {
      case NodeR(v, List()) =>
        v.toString
      case NodeR(v, res) =>
        if(res.length == 1) {
          v.toString + "(" + res.foldRight(")")((x: roseTree[A], acc: String) =>
          string_of_RT(x) + acc)
        } else {

          v.toString + "(" + res.foldRight(")")((x: roseTree[A], acc: String) =>
            if (x eq res.head) {
              string_of_RT(x) + acc
            } else {
              "," + string_of_RT(x) + acc
            }
            )
        }
    }
  }
}

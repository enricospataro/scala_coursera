package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("two ints into an empty heap, findMin gives smaller int") = forAll { (x: Int, y: Int) =>
    val heap = insert(x,insert(y,empty))
    findMin(heap) == math.min(x,y)
  }

  property("element into an empty heap, deleting the minimum gives an empty heap") = forAll { (elem: Int) =>
    val heap = insert(elem,empty)
    isEmpty(deleteMin(heap))
  }

  property("two melded heaps, finding the minimum gives the minimum of one of the two heaps") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1,h2)
    findMin(melded) == math.min(findMin(h1),findMin(h2))
  }

  property("using heaps for sorting") = forAll { (h: H) =>
    def minRec(h: H): List[Int] = h match {
      case empty => List()
      case _ => findMin(h) :: minRec(deleteMin(h))
    }
    def isSorted(l: List[Int]): Boolean = l match {
      case x :: xs => if (x < xs.head) isSorted(xs) else false
      case Nil => true
    }
    isSorted(minRec(h))
  }

}

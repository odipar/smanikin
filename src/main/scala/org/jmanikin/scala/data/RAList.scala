package org.jmanikin.scala.data

/**
 * Purely functional Random Access (RA)List.
 *
 * An RAList is very easy to implement and understand.
 *
 * Another very nice (and underrated!) property is their canonical/unique representation.
 * We leverage that property to calculate the common postfix between two RALists in O(log(n)).
 * 
 * See https://dl.acm.org/doi/abs/10.1145/224164.224187.
 */
object RAList {
  import scala.util.hashing.MurmurHash3._

  trait RATree[A] {
    def first: A
    def second: RATree[A]
    def third: RATree[A]
    def toList: List[A]
    def iterator: Iterator[A]
    def reverseIterator: Iterator[A]
    def hash: Int
    override def hashCode = hash
  }

  case class RAEmptyTree[A]() extends RATree[A] {
    def first = err
    def second = err
    def third = err
    def toList = List()
    def iterator: Iterator[A] = Iterator.empty
    def reverseIterator: Iterator[A] = Iterator.empty
    def err = sys.error("EMPTY TREE")
    def hash = 0
  }

  case class RALeafTree[A](first: A) extends RATree[A] {
    def second = RAEmptyTree()
    def third = RAEmptyTree()
    def iterator: Iterator[A] = Iterator(first)
    def reverseIterator: Iterator[A] = Iterator(first)
    def toList = List(first)
    def hash = mix(first.hashCode, -first.hashCode)
  }

  case class RABinTree[A](first: A, second: RATree[A], third: RATree[A]) extends RATree[A] {
    def toList = first +: (second.toList ++ third.toList)
    def iterator: Iterator[A] = Iterator(first) ++ second.iterator ++ third.iterator
    def reverseIterator: Iterator[A] = third.reverseIterator ++ second.reverseIterator ++ Iterator(first)
    val hash = mix(third.hash, mix(second.hash, first.hashCode()))
  }

  case class RANode[A](size: Long, tree: RATree[A])

  object RAList {
    def apply[A](): RAList[A] = RAEmpty()
  }

  sealed trait RAList[A] {
    def head: RANode[A]
    def tail: RAList[A]
    def depth: Int
    def size: Long
    def isEmpty: Boolean
    def toList: List[A] = iterator.toList
    def iterator = reverse(this).reverse.map(_.tree.iterator).fold(Iterator.empty[A])(_ ++ _)
    def reverseIterator = reverse(this).map(_.tree.reverseIterator).fold(Iterator.empty[A])(_ ++ _)
    def +:(a: A): RAList[A] = {
      if(isEmpty || tail.isEmpty) RACons(RANode(1, RALeafTree(a)), this)
      else {
        val n1 = head
        val n2 = tail.head

        assert(n1.size <= n2.size)

        if (n1.size != n2.size) RACons(RANode(1, RALeafTree[A](a)), this)
        else RACons(RANode(1 + n1.size * 2, RABinTree(a, n1.tree, n2.tree)), tail.tail)
      }
    }

    private def reverse(l: RAList[A]): List[RANode[A]] = {
      var result = List[RANode[A]]() ; var self = l
      while (!self.isEmpty) { result = self.head +: result ; self = self.tail }
      result
    }

    private def reverse(l: List[RANode[A]]): RAList[A]  = {
      var result = RAList[A]() ; var self = l
      while (self.nonEmpty) { result = RACons(self.head, result) ; self = self.tail }
      result
    }

    def prefixList(p: Long): List[A] = prefix(p).toList
    def prefixIterator(p: Long): Iterator[A] = prefix(p).iterator
    def prefixReverseIterator(p: Long): Iterator[A] = prefix(p).reverseIterator

    /* Note that prefix returns a RAList with broken invariants, but we hide this! */
    private def prefix(p: Long): RAList[A] = {
      if (isEmpty || p >= size) this
      else {
        var pre = List[RANode[A]](); var ths = reverse(this).reverse; var pp = p
        while (pp >= ths.head.size) { pre = ths.head +: pre; pp = pp - ths.head.size; ths = ths.tail }
        reverse((pre.reverse ++ prefix(pp, ths.head)).reverse)
      }
    }

    private def prefix(p: Long, node: RANode[A]): List[RANode[A]] = prefix(p, node.size, node.tree)
    private def prefix(p: Long, size: Long, tree: RATree[A]): List[RANode[A]] = {
      if (p == 0) List()
      else if (p >= size) List(RANode(size, tree))
      else if (p == 1) List(RANode(1, RALeafTree(tree.first)))
      else {
        val half = size / 2
        if (p <= half) RANode(1, RALeafTree(tree.first)) +: prefix(p - 1, half, tree.second)
        else RANode(1, RALeafTree(tree.first)) +: (RANode(half, tree.second) +: prefix(p - half - 1, half, tree.third))
      }
    }

    def postfix(p: Long): RAList[A] = {
      if (isEmpty || p >= size) this
      else {
        var pst = List[RANode[A]](); var rvs = reverse(this); var pp = p
        while (pp >= rvs.head.size) {pst = rvs.head +: pst; pp = pp - rvs.head.size; rvs = rvs.tail}
        reverse(pst.reverse ++ postfix(pp, rvs.head))
      }
    }
    private def postfix(p: Long, node: RANode[A]): List[RANode[A]] = postfix(p, node.size, node.tree)
    private def postfix(p: Long, size: Long, tree: RATree[A]): List[RANode[A]] = {
      if (p == 0) List()
      else if (p >= size) List(RANode(size, tree))
      else {
        val half = size / 2

        if (p == (size - 1)) List(RANode(half, tree.third), RANode(half, tree.second))
        else {
          if (p <= half) postfix(p, half, tree.third)
          else RANode(half, tree.third) +: postfix(p - half, half, tree.second)
        }
      }
    }

    def commonPostfix(other: RAList[A]): Long = {
      if (other.size > size) other.commonPostfix(this)
      else {
        val self = postfix(other.size)
        assert(self.size == other.size)
        commonPostfix(self, other)
      }
    }

    private def commonPostfix(l1: RAList[A], l2: RAList[A]): Long = {
      var r1 = reverse(l1)
      var r2 = reverse(l2)
      var s = 0L
      while (r1.nonEmpty && r2.nonEmpty && r1.head.tree == r2.head.tree) {
        assert(r1.head.size == r2.head.size)
        s = s + r1.head.size
        r1 = r1.tail
        r2 = r2.tail
      }
      if (r1.isEmpty) s
      else {
        assert(r1.head.size == r2.head.size)
        s + commonPostfix(r1.head.tree, r2.head.tree, r1.head.size)
      }
    }

    private def commonPostfix(l1: RATree[A], l2: RATree[A], size: Long): Long = {
      if (size == 0) 0
      else if (size == 1) {
        if (l1.first == l2.first) 1
        else 0
      }
      else {
        val half = size / 2

        if (l1.third != l2.third) commonPostfix(l1.third, l2.third, half)
        else if (l1.second != l2.second) half + commonPostfix(l1.second, l2.second, half)
        else if (l1.first != l2.first) size - 1
        else size
      }
    }
  }

  case class RAEmpty[A]() extends RAList[A] {
    def head = err
    def tail = err
    def depth = 0
    def size = 0
    def isEmpty = true
    def err = sys.error("Empty RAList")
  }

  case class RACons[A](head: RANode[A], tail: RAList[A]) extends RAList[A] {
    def depth = 1 + tail.depth
    def size = head.size + tail.size
    def isEmpty = false
  }

  def main(arg: Array[String]): Unit = {
    val x = 100
    var l = RAList[Int]()

    for (i <- 0 until x) l = i +: l

    var r = l
    while (!r.isEmpty) {
      println(r.head.size)
      r = r.tail
    }

    val i = l.iterator
    while (i.hasNext) {
      println("i.next: " + i.next)
    }
  }
}
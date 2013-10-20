package scalax.collection
import scala.collection.mutable.ListBuffer

/** FoldTransformers and the views based on them are a Scala
 *  adaptation, and to some degree an extension, of Rich Hickey's
 *  reducers for Clojure.
 */
object FoldingViews {

  /** The type of left folds */
  type FoldL[A, R] = (R, A) => R

  /** The type of lazy right folds */
  type FoldR[A, R] = (A, => R) => R

  /** A `FoldTransformer` is a transform that
   *  adds a specific operation in front of both a left fold and
   *  a right fold
   */
  abstract class FoldTransformer[-A, +B] extends { self =>

    /** Extend left fold by prefixing it with some operation */
    def left[R] (r: FoldL[B, R]): FoldL[A, R]

    /** Extend lazy right fold by prefixing it with some operation */
    def right[R](r: FoldR[B, R]): FoldR[A, R]

    /** Compose this fold transformer with another one, analogous
     *  to function composition.
     */
    def andThen[C](that: FoldTransformer[B, C]): FoldTransformer[A, C] = new FoldTransformer[A, C] {
      def left[R](r: FoldL[C, R]): FoldL[A, R] = self.left(that.left(r))
      def right[R](r: FoldR[C, R]): FoldR[A, R] = self.right(that.right(r))

      override def fresh: FoldTransformer[A, C] = {
        val self1 = self.fresh
        val that1 = that.fresh
        if ((self1 eq self) && (that1 eq that)) this
        else self1 andThen that1
      }
    }

    /** Return a fresh copy of this fold transformer. The operation
     *  is different from the identity for fold-transformers that
     *  contain an operation that accesses some local mutable state.
     */
    def fresh: FoldTransformer[A, B] = this
  }

  /** A fold transformer that contains a `count` field as local mutable state */
  abstract class CountingFoldTransformer[A, B] extends FoldTransformer[A, B] with Cloneable {
    var count = 0
    override def fresh = clone.asInstanceOf[CountingFoldTransformer[A, B]]
  }

  /** The fold transformer implementing a `map` operation */
  class MapFT[A, B](f: A => B) extends FoldTransformer[A, B] {
    def left[R](r: FoldL[B, R]): FoldL[A, R] =
      (acc, elem) => r(acc, f(elem))
    def right[R](r: FoldR[B, R]): FoldR[A, R] =
      (elem, acc) => r(f(elem), acc)
  }

  /** The fold transformer implementing a `filter` operation */
  class FilterFT[A](p: A => Boolean) extends FoldTransformer[A, A] {
    def left[R](r: FoldL[A, R]): FoldL[A, R] =
      (acc: R, elem: A) => if (p(elem)) r(acc, elem) else acc
    def right[R](r: FoldR[A, R]): FoldR[A, R] =
      (elem, acc) => if (p(elem)) r(elem, acc) else acc
  }

  /** The fold transformer implementing a `flatMap` operation
   *  Note: The function `f` should have type `f: A => Seq[B]` once
   *  views are integrated into Scala collections as a subtype of
   *  `scala.collection.Seq`.
   */
  class FlatMapFT[A, B](f: A => View[B]) extends FoldTransformer[A, B] {
    def left[R](r: FoldL[B, R]): FoldL[A, R] =
      (acc, elem) => f(elem).foldLeft(acc)(r)
    def right[R](r: FoldR[B, R]): FoldR[A, R] =
      (elem, acc) => f(elem).foldRightLzy(acc)(r)
  }

  /** The identity fold transformer */
  class IdentityFT[A] extends FoldTransformer[A, A] {
    def left[R](r: FoldL[A, R]) = {(acc, elem) =>
      //println(s"touching $elem")  // debug
      r(acc, elem)
    }
    def right[R](r: FoldR[A, R]) = {(elem, acc) =>
      //println(s"touching $elem")  // debug
      r(elem, acc)
    }
  }

  /** The fold transformer implementing a `take` operation */
  class TakeFT[A](n: Int) extends CountingFoldTransformer[A, A] {
    def left[R](r: FoldL[A, R]): FoldL[A, R] =
      (acc, elem) =>
        if (count >= n) acc
        else {
          count += 1
          r(acc, elem)
        }
    def right[R](r: FoldR[A, R]): FoldR[A, R] =
      (elem, acc) =>
        if (count >= n) acc
        else {
          count += 1
          r(elem, acc)
        }
  }

  /** The fold transformer implementing a `drop` operation */
  class DropFT[A](n: Int) extends CountingFoldTransformer[A, A] {
    def left[R](r: FoldL[A, R]): FoldL[A, R] =
      (acc, elem) =>
        if (count >= n) r(acc, elem)
        else {
          count += 1
          acc
        }
    def right[R](r: FoldR[A, R]): FoldR[A, R] =
      (elem, acc) =>
        if (count >= n) r(elem, acc)
        else {
          count += 1
          acc
        }
  }

  /** The fold transformer implementing a `takeWhile` operation */
  class TakeWhileFT[A](p: A => Boolean) extends CountingFoldTransformer[A, A] {
    def left[R](r: FoldL[A, R]): FoldL[A, R] = {
      (acc, elem) =>
        count = if (count >= 0 && !p(elem)) -(count + 1) else count + 1
        if (count >= 0) r(acc, elem)
        else acc
    }
    def right[R](r: FoldR[A, R]): FoldR[A, R] = {
      (elem, acc) =>
        count = if (count >= 0 && !p(elem)) -(count + 1) else count + 1
        if (count >= 0) r(elem, acc)
        else acc
    }
  }

  /** The fold transformer implementing a `dropWhile` operation */
  class DropWhileFT[A](p: A => Boolean) extends CountingFoldTransformer[A, A] {
    def left[R](r: FoldL[A, R]): FoldL[A, R] = {
      (acc, elem) =>
        count = if (count >= 0 && !p(elem)) -(count + 1) else count + 1
        if (count < 0) r(acc, elem)
        else acc
    }
    def right[R](r: FoldR[A, R]): FoldR[A, R] = {
      (elem, acc) =>
        count = if (count >= 0 && !p(elem)) -(count + 1) else count + 1
        if (count < 0) r(elem, acc)
        else acc
    }
  }

  /** The fold transformer implementing a `zipWithIndex` operation */
  class ZipWithIndexFT[A] extends CountingFoldTransformer[A, (A, Int)] {
    def left[R](r: FoldL[(A, Int), R]): FoldL[A, R] = {
      (acc, elem) =>
        count += 1
        r(acc, (elem, count - 1))
    }
    def right[R](r: FoldR[(A, Int), R]): FoldR[A, R] = {
      (elem, acc) =>
        count += 1
        r((elem, count - 1), acc)
    }
  }

  /** An instance of type `View[B]` is a lazy sequence of elements of type `B`,
   *  represented by some source sequence and a fold transformer.
   *  Operations that go from view to view are expressed by composing
   *  fold transformers. Operations from view to something  else are
   *  expressed by foldLefts or foldRights.
   */
  abstract class View[B] { self =>

    /** The type of the elements of the source sequence */
    type A

    /** The source sequence */
    val source: Seq[A]

    /** The fold transformer */
    def transformer: FoldTransformer[A, B]

    /** Create new view by composing current fold transformer with a new one */
    def andThen[C](t: FoldTransformer[B, C]) = new View[C] {
      type A = self.A
      val source = self.source
      def transformer: FoldTransformer[A, C] = self.transformer andThen t
    }

    /** The foldLeft operation over this view */
    def foldLeft[C](z: C)(f: (C, B) => C): C = {
      val trans = transformer.fresh
      source.foldLeft(z)(trans.left(f))
    }

    /** The lazy fold right operation over this view */
    def foldRightLzy[C](z: => C)(f: (B, => C) => C): C = {
      val trans = transformer.fresh
      source.foldRightLzy(z)(trans.right(f))
    }

    /** View to view transformation */

    def map[C](f: B => C): View[C] = andThen(new MapFT(f))

    def filter(p: B => Boolean): View[B] = andThen(new FilterFT(p))

    def flatMap[C](f: B => View[C]) = andThen(new FlatMapFT(f))

    def take(n: Int) = andThen(new TakeFT(n))

    def drop(n: Int) = andThen(new DropFT(n))

    def takeWhile(p: B => Boolean) = andThen(new TakeWhileFT(p))

    def dropWhile(p: B => Boolean) = andThen(new DropWhileFT(p))

    def zipWithIndex = andThen(new ZipWithIndexFT[B])

    def partition(p: B => Boolean): (View[B], View[B]) =
      (filter(p), filter(!p(_)))

    /** Going from a view to something else */

    def indexOf(p: B => Boolean) =
      zipWithIndex.foldRightLzy(-1) { (xn, acc) =>
        val (x, n) = xn
        if (p(x)) n else acc
      }

    def find(p: B => Boolean) =
      foldRightLzy(None: Option[B])((elem, alt) => if (p(elem)) Some(elem) else alt)

    def toList: List[B] = foldLeft(new ListBuffer[B])(_ += _).toList

    def toStream: Stream[B] = foldRightLzy(Stream.Empty: Stream[B])(_ #:: _)

    override def toString = //toStream.toString
      s"View(${toList mkString ", "})"
  }

  /** A factory object for views */
  object View {
    def apply[T](xs: Seq[T]) = new View[T] {
      type A = T
      val source = xs
      def transformer = new IdentityFT
    }
  }

  /** A utility decorator that adds `foldRightLzy` to ordinary Scala sequences */
  implicit class LazyRightFolding[A](val s: Seq[A]) extends AnyVal {
    def foldRightLzy[B](z: => B)(f: (A, => B) => B): B = {
      if (s.isEmpty) z
      else f(s.head, s.tail.foldRightLzy(z)(f))
    }
  }

  /** Tentative class to make views evaluate in parallel.
   *  A parallel view is a normal view that has in addition `fold`
   *  and `reduce` operations that might evaluate in parallel.
   */
  abstract class ParView[B] extends View[B] {

    /** System-dependent operation which indicates whether
     *  the computation of this view should be handled by more than one
     *  thread
     */
    def shouldSplit: Boolean = ???

    /** Split view into two halfs which can be treated in parallel */
    def split: (ParView[B], ParView[B]) = ???

    /** Perform to operations in parallel */
    def inParallel[T, U](op1: T, op2: U): (T, U) = ???

    /** Evaluate operation in parallel on this view.
     *  @param  C       The type of the result
     *  @param  z       The identity element of the operation
     *  @param  leftOp  A version of the operation which combines a result with a view element
     *  @param  assocOp A version of the operation which combines two results.
     */
    def fold[C](z: C)(leftOp: (C, B) => C)(assocOp: (C, C) => C): C =
      if (shouldSplit) {
        val (lv, rv) = split
        val (l, r) = inParallel(
          lv.fold(z)(leftOp)(assocOp),
          rv.fold(z)(leftOp)(assocOp)
        )
        assocOp(l, r)
      }
      else foldLeft(z)(leftOp)

    /** Evaluate binary operation on the view element type in parallel.
     *  @param z      The identity element of the operation
     *  @param op     The operation to apply
     */
    def reduce(z: B)(op: (B, B) => B): B = fold(z)(op)(op)

    /** Evaluate in parallel, resulting in Vector */
    def toVector: Vector[B] =
      fold(Vector[B]())(_ :+ _)(_ ++ _)
  }

  /** A decorator for summable parallel collections */
  implicit class Summable(val pv: ParView[Int]) extends AnyVal {
    def sum = pv.reduce(0)(_ + _)
  }
}

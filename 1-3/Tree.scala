sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tr: Tree[A]): Int =
    tr match {
      case Branch(l, r) => size(l) + size(r) + 1
      case Leaf(_) => 1
    }

  def maximum(tr: Tree[Int]): Int =
    tr match {
      case Branch(l, r) => maximum(l) max maximum(r)
      case Leaf(y) => y
    }

  def depth[A](tr: Tree[Int]): Int =
    tr match {
      case Branch(l, r) => depth(l) max depth(r) + 1
      case Leaf(_) => 0
    }

  def map[A, B](tr: Tree[A])(f: A => B): Tree[B] =
    tr match {
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(x) => Leaf(f(x))
    }

  def fold[A, B](tr: Tree[A])(f: A => B)(g: (B, B) => B): B =
    tr match {
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      case Leaf(x) => f(x)
    }

  def sizeViaFold[A](tr: Tree[A]): Int =
    fold(tr)(_ => 1)(_ + _ + 1)

  def maximumViaFold(tr: Tree[Int]): Int =
    fold(tr)(x => x)(_ max _)

  def depthViaFold[A](tr: Tree[Int]): Int =
    fold(tr)(_ => 0)(_ max _ + 1)

  def mapViaFold[A, B](tr: Tree[A])(f: A => B): Tree[B] =
    fold(tr)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
}

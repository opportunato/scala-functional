import strictness.Stream
import strictness.Stream._

Stream(1, 2, 3).toList
Stream(1, 2, 3).toListTail
Stream(1, 2, 3).toListFast

Stream(1, 2, 3).drop(2).toList
Stream(1, 2, 3).take(2).toList
Stream(1, 2, 3, 4).takeWhile(_ <= 3).toList
Stream(1, 2, 3, 4).takeWhileViaFoldRight(_ <= 3).toList

Stream(1, 2, 3, 4).forAll(_ <= 4)
Stream(1, 2, 3, 4).forAll(_ <= 3)

Stream(1, 2).headOption
Stream(1, 2).headOptionViaFoldRight

Stream(1, 2, 3).map(2 * _).toList
Stream(1, 2, 3).filter(_ >= 3).toList
Stream(1, 2, 3).append(Stream(4, 5, 6)).toList
Stream(1, 2, 3).flatMap(x => Stream(x, 2 * x)).toList

val ones: Stream[Int] = constant(1)

ones.take(5).toList
ones.exists(_ % 2 != 0)
ones.map(_ + 1).exists(_ % 2 == 0)
ones.takeWhile(_ == 1)
ones.forAll(_ != 1)

constant(3).take(3).toList
from(2).take(4).toList
fibs.take(10).toList

onesViaUnfold.take(5).toList
constantViaUnfold(3).take(3).toList
fromViaUnfold(2).take(4).toList
fibsViaUnfold.take(10).toList

Stream(1, 2, 3).mapViaUnfold(2 * _).toList
Stream(1, 2, 3).takeViaUnfold(2).toList
Stream(1, 2, 3, 4).takeWhileViaUnfold(_ <= 3).toList

Stream(1, 2, 3).zipWith(Stream(3, 4, 5))(_ + _).toList
Stream(1, 2, 3).zipAll(Stream(1, 2)).toList
Stream(1, 2, 3).zip(Stream(1, 2)).toList

Stream(1, 2, 3).startsWith(Stream(1, 2))
Stream(1, 2, 3).startsWith(Stream(2, 3))

Stream(1, 2, 3).tails.toList.map(_.toList)

Stream(1, 2, 3).hasSubsequence(Stream(1, 2))
Stream(1, 2, 3).hasSubsequence(Stream(2, 3))
Stream(1, 2, 3).hasSubsequence(Stream(2, 3, 4))

Stream(1, 2, 3).scanRight(0)((x, acc) => acc + x).toList
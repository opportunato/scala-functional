import errorhandling.Some
import errorhandling.None
import errorhandling.Option._
import errorhandling.Left
import errorhandling.Right

Some(5) map (x => x)
None map (x => x)
Some(10) flatMap (x => Some(x))
Some(10) getOrElse 0
None getOrElse 0
Some(5) orElse Some(5)
None orElse Some(5)
Some(10) filter (x => x <= 10)
Some(10) filter (x => x < 10)

mean(Seq(1, 2, 3, 4))
mean(Seq())
variance(Seq(1, 2, 3, 4))
variance(Seq())

sequence(List(Some(1), Some(2)))
sequence(List(Some(1), Some(2), None))

sequenceViaTraverse(List(Some(1), Some(2)))
sequenceViaTraverse(List(Some(1), Some(2), None))

Left("Error happened") map (x => x)
Right(5) map (x => 2 * x)

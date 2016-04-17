// TODO make tail recursive
def fibonacci(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, prev: Int, cur: Int): Int =
    if (n == 0) prev
    else go(n-1, cur, prev + cur)
  go(n, 0, 1)
}

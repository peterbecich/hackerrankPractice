// object Solution {
//   def main(args: Array[String]) = MarkToys.main(args)
// }

object MarkToys {

  //               budget                      // number of toys purchased
  def pickCheapest(k: Int, prices: List[Int]): Int =
    if (prices.length > 0 && k > 0) {
      val cheapest = prices.head
      val others = prices.tail
      1+pickCheapest(k - cheapest, others)
    } else 0


  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    var n = sc.nextInt();
    var k = sc.nextInt();
    var prices = new Array[Int](n);
    for(i <- 0 to n-1) {
      prices(i) = sc.nextInt();
    }
    val pricesL = prices.sorted.toList

    val toysBought = pickCheapest(k, pricesL)

    println(toysBought-1)

  }


}

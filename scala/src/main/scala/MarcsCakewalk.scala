
import Math.pow
import Function.uncurried
// object Solution {
//   def main(args: Array[String]) = MarcsCakewalk.main(args)
// }


object MarcsCakewalk {

  val sample = List(1,3,2)

  def calcMiles(cakesEaten: Int)(calories: Int): Int =
    (calories*pow(2, cakesEaten)).toInt

  // def pickCake(miles: Int, cakesEaten: Int, cakes: List[Int])(foo: Int): (Int, List[Int]) = {
  //             miles, cakesEaten, remainingCakes
  val pickCake: ((Int, Int, List[Int])) => (Int) => (Int, Int, List[Int]) =
  {case ((miles, cakesEaten, cakes)) => (foo) => {
    val potentialAdditionalMiles: List[Int] = cakes.map(calcMiles(cakesEaten))
    val zipped: List[(Int,Int)] = cakes.zip(potentialAdditionalMiles)
    val potentialLeastMiles = zipped.sortBy({case (x,y) => y})
    // val leastMiles: (Int, Int) = potentialLeastMiles.head
    val (cals, leastMiles) = potentialLeastMiles.head
    val remainingCakes = potentialLeastMiles.tail.map(_._1)

    (leastMiles+miles, cakesEaten+1, remainingCakes)
  }}


  def pickCake2(miles: Int, cakesEaten: Int, cakes: List[Int]): (Int, Int, List[Int]) = {
    if(cakes.length > 0) {
      val potentialAdditionalMiles: List[Int] = cakes.map(calcMiles(cakesEaten))
      val zipped: List[(Int,Int)] = cakes.zip(potentialAdditionalMiles)
      val potentialLeastMiles = zipped.sortBy({case (x,y) => y}).reverse
      //println(potentialLeastMiles)
      val (cals, leastMiles): (Int, Int) = potentialLeastMiles.head
      val remainingCakes: List[Int] = potentialLeastMiles.tail.map(_._1)
      //println("eat "+cals)
      pickCake2(miles+leastMiles, cakesEaten+1, remainingCakes)
    } else {
      (miles, cakesEaten, cakes)
    }
  }


  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    var n = sc.nextInt();
    var calories = new Array[Int](n);
    for(calories_i <- 0 to n-1) {
      calories(calories_i) = sc.nextInt();
    }

    val caloriesL = calories.toList

    // val out = (1 to 100).foldLeft((0,0,caloriesL))(uncurried(pickCake))

    // println(out)

    val out = pickCake2(0, 0, caloriesL)
    println(out._1)

  }



}

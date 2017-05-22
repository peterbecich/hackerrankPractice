
object Solution {
  def main(args: Array[String]) = MarcsCakewalk.main(args)
}


object MarcsCakewalk {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner (System.in);
    var n = sc.nextInt();
    var calories = new Array[Int](n);
    for(calories_i <- 0 to n-1) {
      calories(calories_i) = sc.nextInt();
    }
    // your code goes here

  }

}

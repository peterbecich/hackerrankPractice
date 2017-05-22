// import java.time._
// import java.time.format.DateTimeFormatter

object TimeParse {
  // val formatter = DateTimeFormatter.ofPattern("H:m")

  val timeString = "12:32 34:01 15:23 9:27 55:22 25:56"

  // val localTimes = timeString.split(' ').map(s => LocalTime.parse(s, formatter))

  def splitTime(s: String): (Int, Int) = s.split(':') match {
    //case Array(h, m, _) => (h.toInt, m.toInt)
    case Array(h, m) => (h.toInt, m.toInt)
  }


  val timeSplit = timeString.split(' ')
  val hoursMinutes = timeSplit.map(splitTime)
  val hoursArr = hoursMinutes.map(_._1)
  val minutesArr = hoursMinutes.map(_._2)

  val minutesDenormalized = minutesArr.sum
  val minutes = minutesDenormalized % 60
  val hoursDenormalized = hoursArr.sum + (minutesDenormalized/60)
  val hours = hoursDenormalized % 24
  val days = hoursDenormalized/24



}

import scala.annotation.tailrec
object Main extends App {

  val days: List[String] = List(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday"
  )

  val tools: Map[String, Double] = Map(
    "Hammer" -> 10.99,
    "Screwdriver" -> 3.0,
    "Drill" -> 10
  )

  def P1a(list: List[String]): String = {
    var name: String = "";
    for (element <- list) {
      name += element + ","
    }
    name = name.dropRight(1)
    return name;
  }

  def P1b(days: List[String]): String = {
    var name = "";

    for (day <- days if day.startsWith("S")) {
      name += day + ",";
    }
    return name;
  }

  def P1c(list: List[String]): String = {
    var str = "";
    var it = list.iterator;
    while (it.hasNext) {
      str += it.next() + ",";
    }
    return str;
  }

  def P2a(lista: List[String]): String = {
    if (lista.isEmpty) {
      return ""
    }
    if (lista.length == 1) {
      return lista.head
    }
    return lista.head + "," + P2a(lista.tail);
  }

  def P2b(lista: List[String]): String = {
    if (lista.isEmpty) {
      return ""
    }
    if (lista.length == 1) {
      return lista.reverse.head
    }
    return lista.reverse.head + "," + P2b(lista.reverse.tail.reverse);
  }

  @tailrec
  def P3(days: List[String], string: String = ""): String = {
    val nextElement = days.head
    val nextResult = string.concat(nextElement + ",")
    if (nextElement == days.last) return nextResult;
    else return P3(days.tail, nextResult);
  }

  def P4a(lista: List[String]): String = {
    return lista.tail.foldLeft(lista.head)((x, y) => x + "," + y)
  }

  def P4b(lista: List[String]): String = {
    return lista
      .foldRight("")((x, y) => {
        x + "," + y
      })
      .dropRight(1)
  }

  def P4c(days: List[String]): String = {
    return days.foldLeft("")((x, y) =>
      x + (if (y.startsWith("S")) y + "," else "")
    );
  }

  def P5(discount: Map[String, Double],
          percent: Double
        ): Map[String, Double] =
    discount.view.mapValues(value => value - value * percent).toMap

  def P6(krotka: (String, Int, Double)) {
    println("Przykład 6 " + krotka._1 + " " + krotka._2 + " " + krotka._3)
  }

  def P7(
          map: Map[String, Double],
          name: String
        ): Option[Double] = map.get(name)

  def P8(list1: List[Int], list2: List[Int] = List()): List[Int] = {
    var nxt = list1.reverseIterator;
    var nxt2 = list2;
    if (nxt.hasNext) {
      var next = nxt.next()
      if (next != 0){
        nxt2 = next :: nxt2
      }
    }
    if (nxt.hasNext) {
      return this.P8(nxt.toList.reverse, nxt2);
    } else
      return list2;
  }

  def P9(numbers: List[Int]): List[Int] = {
    return numbers.map(x => x + 1)
  }

  def P10(numbers: List[Double]): List[Double] = {
    return numbers.filter(x => x <= 12 && x >=(-5)).map(x=>x.abs)
  }

  println("Przykład 1A " + P1a(days))
  println("Przykład 1B " + P1b(days))
  println("Przykład 1C " + P1c(days))
  println("Przykład 2A " + P2a(days))
  println("Przykład 2B " + P2b(days))
  println("Przykład 3 " + P3(days))
  println("Przykład 4A " + P4a(days))
  println("Przykład 4B " + P4b(days))
  println("Przykład 4C " + P4c(days))
  println("Przykład 5" + P5(tools,0.10))
  P6(("Przykład 6" + "Krotka", 7, 7.7))
  println("Przykład 7" + P7(tools, "Hammer").toString);
  println("Przykład 8" + P8(
    List(0, 1, 2, 3, 0, 1, 0 ,10, 2, 0, 0, 3, 0, 1)
  ))
  println("Przykład 9 " + P9(List(1, 2, 3, 4)))
  println("Przykład 10 " + P10(List(85, 60, 3, 4, -1, -8, -9, 12, 166, -166)))
}

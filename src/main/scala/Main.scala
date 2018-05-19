object Main extends App {
  def main(): Unit =  {

  }
  /** Константы */
  val Moscow = List(55.753960, 37.620393)
  val Rostov_on_Don = List(47.2313500, 39.7232800)
  
  /** Вычисление расстояний по координатам */
  def getLen(A : List[Double], B : List[Double]): Double = {
    /** подключаем библиотеку для мат. операций */
    import scala.math._
    /** радиус Земли в метрах */
    val R = 6371210
    /** Функция типа (Double => Double)
      * для перевода из градусов в радианы */
    val toRad = { x : Double => x*(Pi/180) } 
    /** Применяем функцию toRad для каждого
      * элемента списка и записываем в новые
      * списки */
    val Ar = A map(toRad)
    val Br = B map(toRad)
    /** Основная формула для вычисления расстояния */
    acos( sin(Ar(0))*sin(Br(0)) + cos(Ar(0))*cos(Br(0))*cos(Ar(1) - Br(1)) ) * R 
  }
  /** Форматируем результат командой 
    * "% <число символов до запятой> . < число символов после запятой>" format(x) */
  print("%.3f" format(getLen(Moscow, Rostov_on_Don)))
  import scala.collection.Graph // or scalax.collection.mutable.Graph
  val G = Graph(
  "Entry" -> "A",
  "A" -> "B",
  "B" -> "C",
  "B" -> "D",
  "D" -> "F",
  "F" -> "E",
  "E" -> "F",
  "E" -> "C",
  "C" -> "A",
  "C" -> "Exit")
G.dotExport to Console.out
}    

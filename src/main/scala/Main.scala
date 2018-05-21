object Main extends App {
  def main(): Unit =  {

  }
  /** Наверное это все должно хранится в хеш таблице */
  import scala.collection.mutable.HashMap
  import scala.collection.Set
  val City : HashMap[String, List[Double]] = HashMap(
    ("Moscow", List(55.753960, 37.620393)), 
    ("Rostov-on-Don", List(47.2313500, 39.7232800)),
    ("Sankt-Petersburg", List(59.9386300, 30.3141300)))

  /** TODO Написать функцию, создающую HashMap из файла */
  
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
  println("%.3f" format(getLen(City("Moscow"), City("Rostov-on-Don"))))
  /** Библиотеки Graph  */
  import scalax.collection.Graph
  import scalax.collection.GraphPredef._
  import scalax.collection.GraphEdge._
  /** Библиотека подключения графа с весами */
  //import scalax.collection.edge
  import scala.collection.Traversable
  import scalax.collection.edge.WDiEdge
  import scalax.collection.edge.Implicits._
  def initS(){
    /** Список ключей(по факту список городов) */
    val keyCity = City.keySet.toList
    println("keyCity : " + keyCity + "\n")
    /** цикл по элементам keyCity */
    /** yield - означает, что на выходе получим список */
    val edges = for(city <- keyCity) yield keyCity
    /** Записываем все города, кроме city */ 
      .filter( _ != city)
    /** Создаем в каждом списке city~ элемент списка */
      .map(x => city ~ x % getLen(City(city), City(x)))
    println("edges : " + edges + "\n")

    /** Раскрываем списки в один */
    val flatEdges = edges.flatten
    println("flatEdges : " + flatEdges + "\n")
    val G = Graph.from(keyCity, flatEdges)
    /** Выводим города */
      println(G.nodes mkString " ")
    /** Выводим пути между городами */
      println(G.edges mkString " ")
    /** Считаем суммарное расстояние  */
    println(G.totalWeight + "\n")
    def n(outer: String): G.NodeT = G get outer
    val MR = n("Moscow") pathTo n("Rostov-on-Don")
    println(MR)
    val l = MR.get
    println(l)
    print(l.weight)  
  } 
  initS()
}    


//попробуем нагадить
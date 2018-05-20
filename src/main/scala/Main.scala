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
    /** цикл по элементам keyCity */
    /** yield - означает, что на выходе получим список */
    val edges = for(city <- keyCity) yield keyCity
    /** Записываем все города, кроме city */ 
      .filter( _ != city)
    /** Создаем в каждом списке city~ элемент списка */
      .map(x => city ~ x % getLen(City(city), City(x)))
    /** Раскрываем списки в один */
    val flatEdges = edges.flatten
    print(flatEdges)
    val g = Graph(1~2 % 4, 2~3 % 2, 1~>3 % 5, 1~5  % 3,
              3~5 % 2, 3~4 % 1, 4~>4 % 1, 4~>5 % 0)
    val G = Graph.from(keyCity, flatEdges)
      println(G.nodes mkString " ")
      println(G.edges mkString " ")
    /** Считаем суммарное расстояние  */
    print(G.totalWeight)
      
  } 
  initS()
}    

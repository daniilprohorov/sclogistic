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
  import scalax.collection.edge.WUnDiEdge
  import scalax.collection.edge.Implicits._

  /** Функция создания графа */
  def initGraph(Table : HashMap[String, List[Double]] = City, 
            /** Table - таблица типа hashMap 
              * c ключом String, элементом типа List[Double]
              * default = City*/
                getW : ((List[Double], List[Double]) => Double) = getLen
            /** getW - функция, принимающая на вход списки List[Double]
              * возвращающая вес типа Double
              * default = getLen */
                ): Graph[String, WUnDiEdge] = {
            /** Функция возвращает граф c узлами типа String
              * и ребрами, типа Weight UnDirect Edge */ 

    /** Список ключей(по факту список городов) */
    val keyTable = Table.keySet.toList
    /** цикл по элементам keyTable */
    /** yield - означает, что на выходе получим список */
    val edges = for(elem <- keyTable) yield keyTable
    /** Записываем все элементы, кроме elem */ 
      .filter( _ != elem)
    /** Создаем в каждом списке elem ~ элемент списка */
      .map(x => elem ~ x % getW(Table(elem), Table(x)))

    /** Раскрываем списки в один */
    val flatEdges = edges.flatten
    /** Создаем граф из элементов таблицы с
      * вычесленными весами */
    val G = Graph.from(keyTable, flatEdges)
    G
  } 
  
  /** Функция вывода графа в удобночитаемом виде */
  def printGraphText(G : Graph[String, WUnDiEdge]) {
    /** функция, возвращающая узел графа */
    def n(outer: String): G.NodeT = G get outer
    /** записываем в edges все ребра */
    val edges = G.edges.toList
    /** Cчетчик */
    var count = 0
    for(edge <- edges){
      /** Берем любое ребро из узла 1 в узел 2 */
      val temp_edge = n(edge._1) pathTo n(edge._2)
      /** Получаем вес этого ребра */
      val w_edge = temp_edge get 
      val weight = w_edge.weight
      count += 1
      val str = 
      print(f"$count. ${edge._1}%-20s ~ ${edge._2}%-20s Weight = $weight \n")
    }
  }
  
  printGraphText(initGraph())
} 

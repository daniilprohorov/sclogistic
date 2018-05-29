
object Main extends App {
  def main(): Unit =  {

  }

  val city_filename = "city_coordinates.txt"
  /** Наверное это все должно хранится в хеш таблице */
  import scala.collection.mutable.HashMap
  import scala.collection.Set
  import scala.io.Source
  import demo._

  /** Подключение базы данных */
  import scalikejdbc.config.DBs

  DBs.setup('city_db)
  val cityDao = new CityDAO
  val cRelationDao = new City_RelationDAO
  /** Получаем словарь */ 
  val City :  Map[Long, List[Double]] = cityDao.readAll()
    .map( x => Tuple2(x.id.get, List(x.x_cord, x.y_cord)))
    .toMap

  val Relation : Map[Long, List[Tuple2[Long, Double]]]  = cityDao.readAll()
    .map(x => Tuple2(x.id.get, cRelationDao.readCityRelations(x.id)))
    .toMap 

  val CityNames : Map[Long, String] = cityDao.readAll()
    .map( x => Tuple2(x.id.get, x.name))
    .toMap 
  DBs.close('city_db) //если что это закрытие базы
   
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
  def initGraph(TableCity : Map[Long, List[Double]] = City,
            /** TableCity - таблица типа hashMap 
              * default = City*/
                TableRelation : Map[Long, List[Tuple2[Long, Double]]] = Relation, 
            /** TableRelation - таблица типа hashMap                           
              * c ключом Long, элементом типа List[String]                     
              * default = Relation*/                                           
                getW : ((List[Double], List[Double]) => Double) = getLen       
            /** getW - функция, принимающая на вход списки List[Double]      
              * возвращающая вес типа Double                                 
              * default = getLen */                                             
                ): Graph[Long, WUnDiEdge] = {                                  
            /** Функция возвращает граф c узлами типа Long                     
              * и ребрами, типа Weight UnDirect Edge */                        
                                                
    /** Список ключей городов */
    val keyCity = TableCity.keySet.toList
    /** цикл по элементам keyTable */                                                
    /** yield - означает, что на выходе получим список */                            
    val edges = for(elem <- keyCity) yield TableRelation(elem) 
    /** Создаем в каждом списке elem ~ элемент списка */                             
      .map(x => elem ~ x._1 % getW(TableCity(elem), TableCity(x._1)))
  
    /** Раскрываем списки в один */
    val flatEdges = edges.flatten                                                    
    /** Создаем граф из элементов таблицы с                                          
      * вычесленными весами */
    val G = Graph.from(keyCity, flatEdges)
    G
  } 


  /** Функция вывода графа в удобночитаемом виде */
  def printGraphText(G : Graph[Long, WUnDiEdge], Name: Map[Long, String] = CityNames) {
    /** функция, возвращающая узел графа */
    def n(outer: Long): G.NodeT = G get outer
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
      print(f"$count. ${Name(edge._1)}%-20s ~ ${Name(edge._2)}%-20s Weight = $weight \n")
    }
  }
  
  printGraphText(initGraph())
} 

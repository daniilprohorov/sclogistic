package GR

class OurGraph{
  /** Библиотеки Graph  */
  type CityT = Map[Long, List[Double]]
  type RelationT = Map[Long, List[Tuple2[Long, Double]]]
  type NamesT = Map[Long, String]

  import scalax.collection.mutable.Graph
  import scalax.collection.GraphPredef._
  import scalax.collection.edge.WUnDiEdge
  import scalax.collection.edge.Implicits._
  import Database.DatabaseApp
  import scala.language.postfixOps
  
  /** Получаем словарь (map) */
  
  type CityGraphT = Graph[Long, WUnDiEdge]

  val Temp_DataBase = new DatabaseApp
  val DataBase = Temp_DataBase.DataBase

  val city = DataBase.city()
  val relation = DataBase.relation()
  val names = DataBase.names()


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



  /** Функция создания графа */   
  def initGraph(TableCity : CityT = city,
            /** TableCity - таблица типа hashMap 
              * default = city*/
                TableRelation : RelationT = relation,
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
  def printGraphText(G : CityGraphT = initGraph(), Name: NamesT  = names) {
    /** функция, возвращающая узел графа */
    def n(outer: Long): G.NodeT = G get outer
    /** записываем в edges все ребра */
    val edges = G.edges.toList
    /** Cчетчик */
    var count = 0
    for(edge <- edges){

      val weight = edge.weight
      count += 1
      printf(f"$count. ${Name(edge._1)+" = " + edge._1.toString}%-20s ~ ${Name(edge._2)+" = " + edge._2.toString}%-20s Weight = $weight \n")
    }
  }
  val inf = 9999999
  case class DijkstraNode(val id : Long,var mark : Boolean = false,  var weight : Double = inf, var way : List[Long] = List())
  type DijkstraGraphT = Graph[DijkstraNode, WUnDiEdge]

  def dr(Start : Long, G : CityGraphT) : DijkstraGraphT = {
    def n(outer: Long): G.NodeT = G get outer

    def takeWeight(edge : G.EdgeT) : Double = edge.weight
    /** записываем в edges все ребра */
    val edges = G.edges.toList 
    val dijkstraGraph = for( edge <- edges ) yield
      if(edge._1 == Start){
        (DijkstraNode(Start, weight = 0) ~ DijkstraNode(edge._2) % takeWeight(edge)) 
      }
      else if(edge._2 == Start){
        (DijkstraNode(edge._1) ~ DijkstraNode(Start, weight = 0) % takeWeight(edge)) 
      }
      else {
        (DijkstraNode(edge._1) ~ DijkstraNode(edge._2) % takeWeight(edge)) 
      }

    val nodesG = G.nodes.toList
    /** Создаем начальный граф */
    val nodesGraph = nodesG.map(x => if(x == Start)DijkstraNode(Start, weight = 0) else DijkstraNode(x)) 

    val Lg = Graph.from(nodesGraph, dijkstraGraph) 
    //for(node <- Lg.nodes)if(node.id == Start){node.weight = 0}

    def nlg(outer: DijkstraNode): Lg.NodeT = Lg get outer

    def _rec(elem : DijkstraNode, rg : DijkstraGraphT = Lg, listEdges : Set[WUnDiEdge[DijkstraNode]] = Set()) : DijkstraGraphT = {
      import scala.collection.mutable.Queue
      def n(outer: DijkstraNode): rg.NodeT = rg get outer
      def e(outer: WUnDiEdge[DijkstraNode]): rg.EdgeT = rg get outer
      val node = n(elem)

      def takeWeight(A : DijkstraNode, B : DijkstraNode) : Double = {
        val temp_edge = n(A) pathTo n(B)
        /** Получаем вес этого ребра */
        val w_edge  = temp_edge get 
        val weight = w_edge.weight
        weight 
      }

      if(node.mark == false) {
          val list_nodes = node.diSuccessors.filter(_.mark == false)

          for(second_node <- list_nodes){
              val weight =  takeWeight(elem, second_node)
              if(weight + node.weight < second_node.weight){
                  second_node.weight = weight + node.weight  
              }
          } 

          node.mark = true
          val listWithEdges : Set[WUnDiEdge[DijkstraNode]] = (listEdges ++ node.outgoing.map(_.toOuter)).filter(x => x._1.mark == false || x._2.mark  == false)
          if(listWithEdges.isEmpty){
            return rg
          }
          val edgeNext = listWithEdges.minBy(_.weight)


          val (nodeNext, nodeLast) = edgeNext match {
            case x if x._1.mark == true => Tuple2(x._2, x._1)
            case x if x._2.mark == true => Tuple2(x._1, x._2)
          }
          rg.nodes.filter(_.id == nodeNext.id).map(x => x.way = nodeLast.id :: nodeLast.way) 
          val h = Graph.empty[DijkstraNode,WUnDiEdge] ++ rg
          _rec(nodeNext, h, listWithEdges)

      }
      else {
      rg 
      }
    }


    val out = _rec(DijkstraNode(Start, weight = 0.0D), Lg)
    for(node <- out.nodes) node.way = node.way.reverse
    val out_reverse = Graph.empty[DijkstraNode,WUnDiEdge] ++ out 
    out_reverse
  }

  def best_way(A : Long, B : Long, G : CityGraphT, 
    func_min_way : (Long, CityGraphT) => DijkstraGraphT = dr ) : List[Long] =
    {
      val g = dr(A, G)
      val way = g.nodes.filter(_.id == B).head.way
      way

  }

}

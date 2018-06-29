
object Main extends App {
    
  import GR._  
  val G = new OurGraph
  val g = G.initGraph()
  G.printGraphText()
  G.dr(1, g)
  val way = G.best_way_dijkstra(1, 13, g)._2
  var graph = G.best_way_dijkstra(1, 13, g)._1 
  print("\n\n")
  G.printGraphText()  
  print("\n\n")
  G.printGraphText(graph)  
  println(way) 
  G.loop(g)
} 

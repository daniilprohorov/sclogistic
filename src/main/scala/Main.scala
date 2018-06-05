
object Main extends App {
    
  import GR._  
  val G = new OurGraph
  val g = G.initGraph()
  G.printGraphText()
  G.dr(1, g)
} 

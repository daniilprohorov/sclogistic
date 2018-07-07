package Module_scalafx



import Database.DatabaseApp
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Line
import scalafx.scene.paint.Color


object Scene_App extends JFXApp{

  val Temp_DataBase = new DatabaseApp
  val DataBase = Temp_DataBase.DataBase

  val city = DataBase.city()
  val relation = DataBase.relation()
  val names = DataBase.names()
  val CityBoxApp = new CityBox
  val dispersion = 180
  val startCity = (city(1)(1), city(1)(0))
  /** Long, Map[Long, List[Double, Double]], Int, Tuple2[Double, Double] => Tuple2[Double, Double] */
  def getXY(id : Long, 
      city: Map[Long, List[Double]] = city, 
      dispersion : Int = dispersion, 
      startCity : Tuple2[Double, Double] = startCity) : Tuple2[Double, Double] = {
    Tuple2(((city(id)(1) - startCity._1) * dispersion + 500).toDouble, ( -1 * (city(id)(0) - startCity._2) * dispersion * 4 + 400).toDouble)
  }

    stage = new PrimaryStage {
    width = 800
    height = 800
    centerOnScreen
    title = "ScalaFX Hello World"
    scene = new Scene {
      fill = Black
      val road = new Line {
          startX = 10
          startY = 10
          endX = 500
          endY = 500
          fill = Color.Red
          stroke = Color.Blue
          strokeWidth = 10d }
        val cities = city.keySet.map(x => CityBoxApp.getBoxes(names(x), getXY(x)._1, getXY(x)._2)).toList

        content = new Pane {
          padding = Insets(80)
          children = road :: cities  
        } 
      }
    }
}

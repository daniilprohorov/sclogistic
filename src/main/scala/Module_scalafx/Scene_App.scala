package Module_scalafx



import Database.DatabaseApp
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color._

object Scene_App extends JFXApp{

  val Temp_DataBase = new DatabaseApp
  val DataBase = Temp_DataBase.DataBase

  val city = DataBase.city()
  val relation = DataBase.relation()
  val names = DataBase.names()
  val CityBoxApp = new CityBox
  val dispersion = 180

  stage = new PrimaryStage {
    width = 800
    height = 800
    centerOnScreen
    title = "ScalaFX Hello World"
    scene = new Scene {
      fill = Black
      content = new Pane {
        padding = Insets(80)
        children = city.keySet.map(x => CityBoxApp.getBoxes(names(x), (city(x)(1) - city(1)(1)) * dispersion + 500, -1 * (city(x)(0) - city(1)(0)) * dispersion * 4 + 400)).toSeq
      }
    }
  }
}

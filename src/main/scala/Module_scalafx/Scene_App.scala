package Module_scalafx



import Database.DatabaseApp
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color._

object Scene_App extends JFXApp{

  val Temp_DataBase = new DatabaseApp
  val DataBase = Temp_DataBase.DataBase

  val сity = DataBase.city()
  val relation = DataBase.relation()
  val names = DataBase.names()
  val CityBoxApp = new CityBox

  stage = new PrimaryStage {
    title = "ScalaFX Hello World"
    scene = new Scene {
      fill = Black
      content = new HBox {
        padding = Insets(20)
        children = names.values.map(CityBoxApp.getBoxes(_)).toSeq
      }
    }
  }
}

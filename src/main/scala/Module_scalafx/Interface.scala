package Module_scalafx

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color._


object Interface extends JFXApp{
  stage = new PrimaryStage {
    title = "ScalaFX Hello World"
    scene = new Scene {
      fill = Black
      content = new HBox {
        padding = Insets(20)
        children = Seq()
      }
    }
  }
}

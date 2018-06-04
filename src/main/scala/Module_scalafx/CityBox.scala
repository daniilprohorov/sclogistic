package Module_scalafx


import scalafx.geometry.Insets
import scalafx.scene.control.RadioButton
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{LinearGradient, Stops}
import scalafx.scene.text.Text
import scalafx.geometry.Pos
/** Получаем словарь (map) */


class CityBox {

  def getBoxes (name: String, x : Double, y : Double) =
  new VBox {
    //padding = Insets(40)
    layoutX = x
    layoutY = y
    alignment = Pos.Center
    children = Seq(
      new Text {
        text = name
        style = "-fx-font-size: 12pt"
        fill = new LinearGradient(
          endX = 0,
          stops = Stops(PaleGreen, SeaGreen)
        )
      },
      new RadioButton {
        text = ""
      }
    )
  }
}

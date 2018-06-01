package Module_scalafx


import scalafx.geometry.Insets
import scalafx.scene.control.RadioButton
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{LinearGradient, Stops}
import scalafx.scene.text.Text
import scalafx.geometry.Pos
import Database.DatabaseApp

/** Получаем словарь (map) */


class City {

  val Temp_DataBase = new DatabaseApp
  val DataBase = Temp_DataBase.DataBase

  val city = DataBase.city()
  val relation = DataBase.relation()
  val names = DataBase.names()

  new VBox {
    padding = Insets(20)
    alignment = Pos.Center
    children = Seq(
      new Text {
        text = ""
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

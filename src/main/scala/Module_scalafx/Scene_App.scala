package Module_scalafx


import javafx.animation.Animation.Status

import scala.language.postfixOps
import scalafx.Includes._
import scalafx.animation.{Interpolator, Timeline}
import scalafx.beans.property.DoubleProperty

import Database.DatabaseApp
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout.{Pane, HBox}
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Line
import scalafx.scene.paint.Color
import scalafx.scene.control.Button
import scalafx.util.Duration


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

 val endXVal = DoubleProperty(100.0)
 val endYVal = DoubleProperty(100.0)

  val anim = new Timeline {
    //autoReverse = true
    keyFrames = Seq(
      at(0 s) {
        endXVal -> 100 
      },
      at(0 s) {
        endYVal -> 100 
      },
      at(4 s) {
        endXVal -> 300 tween Interpolator.Linear
      },
      at(4 s) {
        endYVal -> 300 tween Interpolator.Linear
      })
    cycleCount = 1//Timeline.Indefinite
  }

    stage = new PrimaryStage {
    width = 800
    height = 800
    centerOnScreen
    title = "ScalaFX Hello World"
    scene = new Scene {
      fill = Black
      val road = new Line {
          startX =  100  
          startY = 100
          endX <== endXVal 
          endY <== endYVal 
          fill = Color.Red
          stroke = Color.Blue
          strokeWidth = 10d }
        val cities = city.keySet.map(x => CityBoxApp.getBoxes(names(x), getXY(x)._1, getXY(x)._2)).toList
        val buttons = new HBox {
          layoutX = 60
          layoutY = 420
          spacing = 10
          children = List(
            new Button {
              text = "Start"
              onAction = handle {anim.playFromStart()}
              disable <== anim.status =!= Status.STOPPED
            },
            new Button {
              text = "Pause"
              onAction = handle {anim.pause()}
              disable <== anim.status =!= Status.RUNNING
            },
            new Button {
              text = "Resume"
              onAction = handle {anim.play()}
              disable <== anim.status =!= Status.PAUSED
            },
            new Button {
              text = "Stop"
              onAction = handle {anim.stop()}
              disable <== anim.status === Status.STOPPED
            }
          )
        }

        content = new Pane {
          padding = Insets(80)
          children =buttons :: road :: cities  
        } 
      }
    }
}

package Database

case class City(id: Option[Long] = None,
                name: String,
                x_cord: Double,
                y_cord: Double)
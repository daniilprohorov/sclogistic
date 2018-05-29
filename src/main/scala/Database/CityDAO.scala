package Database

import scalikejdbc._

class CityDAO extends DbConnected {
  /*def create(cityToSave: City): Long = {
    insideLocalTx { implicit session =>
      val cityId: Long =
        sql"""INSERT INTO cities (city, x_cord, y_cord)
             VALUES (${cityToSave.name}, ${cityToSave.x_cord}, ${cityToSave.y_cord})"""
          .updateAndReturnGeneratedKey().apply()
      cityId
    }
  }*/
  def read(cityId: Long) : Option[City] = {
    insideReadOnly { implicit session =>
      sql"SELECT * FROM cities WHERE id = ${cityId}".map(rs =>
        City(rs.longOpt("id"),
          rs.string("city"),
          rs.double("x_cord"),
          rs.double("y_cord")))
        .single.apply()
    }
  }
  def readAll() : List[City] = {
    insideReadOnly { implicit session =>
      sql"SELECT * FROM cities".map(rs =>
        City(rs.longOpt("id"),
          rs.string("city"),
          rs.double("x_cord"),
          rs.double("y_cord")))
        .list.apply()
    }
  }
  /*def update(cityToSave: City) : Unit = {
    insideLocalTx { implicit session =>
      sql"""UPDATE cities SET
                city=${cityToSave.name},
                x_cord=${cityToSave.x_cord},
                y_cord=${cityToSave.y_cord}
              WHERE id = ${cityToSave.id}
          """.execute().apply()
    }
  }
  def delete(cityId: Long) : Unit= {
    insideLocalTx { implicit session =>
      sql"DELETE FROM t_cities WHERE id = ${cityId}".execute().apply()
    }
  }*/
}
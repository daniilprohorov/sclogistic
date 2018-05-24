package demo

class Demo_app extends App {
  DBs.setup('city_db)
  val cityDao = new CityDAO

  println(cityDao)
  DBs.close('city_db)
}

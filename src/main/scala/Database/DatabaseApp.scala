package Database

class DatabaseApp {
  type CityT = Map[Long, List[Double]]
  type RelationT = Map[Long, List[Tuple2[Long, Double]]]
  type NamesT = Map[Long, String]

  class BD(name : Symbol = 'city_db){
    import scalikejdbc.config.DBs

    val cityDao = new CityDAO
    val cRelationDao = new City_RelationDAO

    def getCity(): CityT = {
      DBs.setup(name)
      cityDao.readAll()
        .map( x => Tuple2(x.id.get, List(x.x_cord, x.y_cord)))
        .toMap
    }

    def getRelation() : RelationT = {
      DBs.setup(name)
      cityDao.readAll()
        .map(x => Tuple2(x.id.get, cRelationDao.readCityRelations(x.id)))
        .toMap
    }

    def getNames() : NamesT = {
      DBs.setup(name)
      cityDao.readAll()
        .map( x => Tuple2(x.id.get, x.name))
        .toMap
    }

    DBs.close(name)
  }

  object DataBase {
    val DataBaseVal = new BD
    def city() : CityT  = DataBaseVal.getCity()
    def relation() : RelationT = DataBaseVal.getRelation()
    def names() : NamesT = DataBaseVal.getNames()
  }
}

package demo

import scalikejdbc._

class City_RelationDAO extends DbConnected {
  def readOne(Idcity_1: Long) : Option[City_Relation] = {
    insideReadOnly { implicit session =>
      sql"SELECT * FROM relations WHERE idRelations = ${Idcity_1}".map(rs =>
        City_Relation(rs.longOpt("idRelations"),
                      rs.long("Idcity_1"),
                      rs.long("Idcity_2"),
                      rs.double("Coef")))
        .single.apply()
    }
  }
  def readAll() : List[City_Relation] = {
    insideReadOnly { implicit session =>
      sql"SELECT * FROM realations".map(rs =>
        City_Relation(rs.longOpt("idRelations"),
                      rs.long("Idcity_1"),
                      rs.long("Idcity_2"),
                      rs.double("Coef")))
        .list.apply()
    }
  }
  def readAllCitiesRelations() : List[City_Relation] = {
    insideReadOnly { implicit session =>
      sql"SELECT * FROM cities, realations WHERE cities.id = relations.Idcity_1".map(rs =>
        City_Relation(rs.longOpt("idRelations"),
          rs.long("Idcity_1"),
          rs.long("Idcity_2"),
          rs.double("Coef")))
        .list.apply()
    }
  }
}

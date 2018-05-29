package Database

import scalikejdbc._

class City_RelationDAO extends DbConnected {
  def readOne(idRelations: Long) : Option[City_Relation] = {
    insideReadOnly { implicit session =>
      sql"SELECT * FROM relations WHERE idRelations = ${idRelations}".map(rs =>
        City_Relation(rs.longOpt("idRelations"),
                      rs.long("Idcity_1"),
                      rs.long("Idcity_2"),
                      rs.double("Coef")))
        .single.apply()
    }
  }
  def readAll() : List[City_Relation] = {
    insideReadOnly { implicit session =>
      sql"SELECT * FROM relations".map(rs =>
        City_Relation(rs.longOpt("idRelations"),
                      rs.long("Idcity_1"),
                      rs.long("Idcity_2"),
                      rs.double("Coef")))
        .list.apply()
    }
  }
  def readCityRelations(Idcity_1: Option[Long]) : List[Tuple2[Long, Double]] = {
    insideReadOnly { implicit session =>
      sql"SELECT Idcity_2, Coef FROM relations WHERE ${Idcity_1}= Idcity_1".map(rs =>
        Tuple2(rs.long("Idcity_2"),
               rs.double("Coef")))
        .list.apply()
    }
  }
}

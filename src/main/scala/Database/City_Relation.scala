package Database

case class City_Relation(idRelations: Option[Long] = None,
                         Idcity_1: Long,
                         Idcity_2: Long,
                         Coef: Double)
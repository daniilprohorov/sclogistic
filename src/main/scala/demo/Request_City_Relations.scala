package demo

case class Request_City_Relations (id: Option[Long] = None,
                                   name: String,
                                   x_cord: Double,
                                   y_cord: Double,
                                   idRelations: Option[Long] = None,
                                   Idcity_1: Long,
                                   Idcity_2: Long,
                                   Coef: Double)

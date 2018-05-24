package demo

import java.sql.Connection

import scalikejdbc.{ConnectionPool, DB, DBSession, _}

trait DbConnected {
  def connectionFromPool : Connection = ConnectionPool.borrow('city_db) // (1)
  def dbFromPool : DB = DB(connectionFromPool)				// (2)

  def insideLocalTx[A](sqlRequest: DBSession => A): A = {		// (3)
    using(dbFromPool) { db =>
      db localTx { session =>
        sqlRequest(session)
      }
    }
  }

  def insideReadOnly[A](sqlRequest: DBSession => A): A = {		// (4)
    using(dbFromPool) { db =>
      db readOnly { session =>
        sqlRequest(session)
      }
    }
  }
}
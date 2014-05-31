package com.matrix.salat.test

import com.novus.salat.global._
import com.novus.salat.annotations._
import com.matrix.salat.Imports._
import com.mongodb.casbah.MongoConnection
import com.novus.salat.dao.SalatDAO

case class User(@Key("_id") id: Long, name: String, prefSize: Int, tags: List[String]) {}

object UserDao extends SalatDAO[User, Long](collection = MongoConnection()("dsl")("user")) 
with BaseDao[User, Long]

object TestDao {
  def main(args: Array[String]) = {
    UserDao.count()
    UserDao.insert(User(1, "user1", 1, List("tag", "tago")))
    UserDao.insert(User(2, "user2", 2, List("tag")))

    val users = UserDao.where(u => (u.tags $contains "tag") || (u.id $eqs 1)).find().toList

    for(u <- users) {
      println(u.id + "," + u.name + "," + u.prefSize)
    }

    UserDao.where(_.id $eqs 2).update { u => (u.tags $push "abc") ~ (u.prefSize $inc 1) }
    println(UserDao.where(_.id $gte 0).count())
  }
}
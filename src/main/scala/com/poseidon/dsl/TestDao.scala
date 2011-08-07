package com.poseidon.dsl

import com.novus.salat._
import com.novus.salat.global._
import com.novus.salat.annotations._
import com.novus.salat.dao._
import com.mongodb.casbah.Imports._
import com.poseidon.dsl.QueryBuilder._

case class User(@Key("_id") id: Long, name: String, prefSize: Int, tags: List[String]) {
}

object UserDao extends SalatDAO[User, Long](collection = MongoConnection()("test_db")("test_coll")) 
with BaseDao[User, Long]

object TestDao {
  def main(args: Array[String]) = {
    val _id = UserDao.insert(User(1, "user1", 1, List("tag", "tago")))
    UserDao.insert(User(2, "user2", 2, List("tag")))
    
    val users = UserDao.where(u => (u.tags $contains "tag") || (u.id eqs 1)).find().toList
    
    for (u <- users) {
      println(u.id + "," + u.name + "," + u.prefSize)
    }
    
    UserDao.where(_.id eqs 2).update {u => (u.tags push "abc")~(u.prefSize inc 1)}
    println(UserDao.where(_.id gte 0).count)
  }
}
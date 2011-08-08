package com.poseidon.dsl

import com.novus.salat._
import com.novus.salat.dao.SalatDAO
import com.mongodb.DBObject
import com.novus.salat.dao.SalatMongoCursor
import com.mongodb.casbah.commons.MongoDBObject
import com.poseidon.util.Context
import com.mongodb.WriteConcern

class WhereBuilder[ObjectType <: CaseClass, ID <: Any](dao: BaseDao[ObjectType, ID], q: DBObject){
  def ids(): List[ID] = {
    dao.ids(q)
  }
  
  def find(): SalatMongoCursor[ObjectType] = {
    dao.find(q)
  }
  
  def findOne(): Option[ObjectType] = {
    dao.findOne(q)
  }
  
  def count(): Long = {
    dao.count(q)
  }
  
  def remove() = {
    dao.remove(q)
  }
  
  def remove(wc: WriteConcern) {
    dao.remove(q, wc)
  }
  
  def update(upsert: Boolean, multi: Boolean, wc: WriteConcern)(uo: ObjectType => ModifyClause)(implicit m: Manifest[ObjectType]): Unit = {
    dao.update(q, uo, upsert, multi, wc, m)
  }
  
  def update(uo: ObjectType => ModifyClause)(implicit m: Manifest[ObjectType]): Unit = {
    dao.update(q, uo, true, true, WriteConcern.NORMAL, m)
  }
}

trait BaseDao[ObjectType <: CaseClass, ID <: Any]{
  def ids[A <% DBObject](query: A): List[ID]
  def find[A <% DBObject](ref: A): SalatMongoCursor[ObjectType]
  def find[A <% DBObject, B <% DBObject](ref: A, keys: B): SalatMongoCursor[ObjectType]

  def findOne[A <% DBObject](t: A): Option[ObjectType]
  def count(q: DBObject = MongoDBObject(), fieldsThatMustExist: List[String] = Nil, fieldsThatMustNotExist: List[String] = Nil): Long

  def remove[A <% DBObject](q: A)
  def remove[A <% DBObject](q: A, wc: WriteConcern)

  def update[A <% DBObject, B <% DBObject](q: A, o: B, upsert: Boolean, multi: Boolean, wc: WriteConcern)
  
  def where(q: ObjectType => QueryClause)(implicit m: Manifest[ObjectType]): WhereBuilder[ObjectType, ID] = {
    val (context, entry) = Context.create[ObjectType](m.erasure)
    val qc = q(entry)
    val qo = qc.toDBObject(context)
    new WhereBuilder(this, qo)
  }
  
  def update(q: DBObject, uo: ObjectType => ModifyClause, upsert: Boolean, multi: Boolean, wc: WriteConcern, m: Manifest[ObjectType]): Unit = {
    val (context, entry) = Context.create[ObjectType](m.erasure)
    val uc = uo(entry)
    val udbo = uc.toDBObject(context)
    update(q, udbo, upsert, multi, wc)
  }
}
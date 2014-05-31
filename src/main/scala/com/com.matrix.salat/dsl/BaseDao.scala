package com.matrix.salat.dsl

import com.novus.salat._
import com.novus.salat.dao.BaseDAOMethods
import com.matrix.salat.util.Context
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.Imports
import com.novus.salat
import com.mongodb.WriteConcern
import com.novus.salat.dao.SalatMongoCursor

class WhereBuilder[ObjectType <: CaseClass, ID <: Any](dao: BaseDao[ObjectType, ID], q: DBObject){
  implicit val ident = { x: ObjectType => x}

  def count(fieldsThatMustExist: List[String] = Nil, fieldsThatMustNotExist: List[String] = Nil): Long = {
    dao.count(q, fieldsThatMustExist, fieldsThatMustNotExist, dao.defaultReadPreference)
  }

  def ids(): List[ID] = {
    dao.ids(q)
  }

  def findOne(rp: Imports.ReadPreference): Option[ObjectType] = {
    dao.findOne(q, rp)
  }

  def projection[P <: CaseClass](field: String)(implicit m: Manifest[P], ctx: salat.Context): Option[P] = {
    dao.projection(q, field)
  }

  def projections[P <: CaseClass](field: String)(implicit m: Manifest[P], ctx: salat.Context): List[P] = {
    dao.projections(q, field)
  }

  def primitiveProjections[P](field: String)(implicit m: Manifest[P], ctx: salat.Context): List[P] = {
    dao.primitiveProjections(q, field)
  }

  def primitiveProjection[P](field: String)(implicit m: Manifest[P], ctx: salat.Context): Option[P] = {
    dao.primitiveProjection(q, field)
  }

  def find(fields: List[String] = Nil, rp: Imports.ReadPreference = dao.defaultReadPreference): SalatMongoCursor[ObjectType] = {
    val keys = new BasicDBObject
    for(field <- fields) keys.put(field, 1)
    dao.find(q, keys)
  }

  def remove(wc: WriteConcern): Imports.WriteResult = {
    dao.remove(q, wc)
  }

  def update(upsert: Boolean, multi: Boolean, wc: WriteConcern)(uo: ObjectType => ModifyClause)(implicit m: Manifest[ObjectType]): Unit = {
    dao.update(q, uo, upsert, multi, wc, m)
  }
  
  def update(uo: ObjectType => ModifyClause)(implicit m: Manifest[ObjectType]): Unit = {
    dao.update(q, uo, true, true, WriteConcern.NORMAL, m)
  }
}

trait BaseDao[ObjectType <: CaseClass, ID <: Any] extends BaseDAOMethods[ObjectType, ID] {
  def where(q: ObjectType => QueryClause)(implicit m: Manifest[ObjectType]): WhereBuilder[ObjectType, ID] = {
    val (context, entry) = Context.create[ObjectType](m.runtimeClass)
    val qc = q(entry)
    val qo = qc.toDBObject(context)
    new WhereBuilder(this, qo)
  }

  def update(q: DBObject, uo: ObjectType => ModifyClause, upsert: Boolean, multi: Boolean, wc: WriteConcern, m: Manifest[ObjectType]): Unit = {
    val (context, entry) = Context.create[ObjectType](m.runtimeClass)
    val uc = uo(entry)
    val udbo = uc.toDBObject(context)
    update(q, udbo, upsert, multi, wc)
  }
}
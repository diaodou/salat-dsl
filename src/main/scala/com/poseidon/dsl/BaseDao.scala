package com.poseidon.dsl

import com.novus.salat._
import com.novus.salat.dao.SalatDAO
import com.mongodb.DBObject
import com.novus.salat.dao.SalatMongoCursor
import com.mongodb.casbah.commons.MongoDBObject
import net.sf.cglib.proxy.Enhancer
import com.poseidon.util.Context
import com.novus.salat.annotations.raw.Key
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
  val context = new Context()
  
  def ids[A <% DBObject](query: A): List[ID]
  def find[A <% DBObject](ref: A): SalatMongoCursor[ObjectType]
  def find[A <% DBObject, B <% DBObject](ref: A, keys: B): SalatMongoCursor[ObjectType]

  def findOne[A <% DBObject](t: A): Option[ObjectType]
  def count(q: DBObject = MongoDBObject(), fieldsThatMustExist: List[String] = Nil, fieldsThatMustNotExist: List[String] = Nil): Long

  def remove[A <% DBObject](q: A)
  def remove[A <% DBObject](q: A, wc: WriteConcern)

  def update[A <% DBObject, B <% DBObject](q: A, o: B, upsert: Boolean, multi: Boolean, wc: WriteConcern)
  
  def where(q: ObjectType => QueryClause)(implicit m: Manifest[ObjectType]): WhereBuilder[ObjectType, ID] = {
    val fieldNameMap = m.erasure.getMethods()
    		.filter(_.isAnnotationPresent(classOf[Key]))
    		.map {m => (m.getName(), m.getAnnotation(classOf[Key]).value())}
    		.toMap
    context.setFieldNameMap(fieldNameMap)
    val enhancer = new Enhancer()
    enhancer.setSuperclass(m.erasure)
    enhancer.setCallback(context)
    val constr = m.erasure.getConstructors()(0)
    val args: Array[Object] = constr.getParameterTypes().map(x => if(x.isPrimitive()) java.lang.reflect.Array.get(java.lang.reflect.Array.newInstance(x, 1), 0).asInstanceOf[Object] else null)
    val p = enhancer.create(constr.getParameterTypes(), args).asInstanceOf[ObjectType]
    val qc = q(p)
    val qo = qc.toDBObject(context)
    new WhereBuilder(this, qo)
  }
  
  def update(q: DBObject, uo: ObjectType => ModifyClause, upsert: Boolean, multi: Boolean, wc: WriteConcern, m: Manifest[ObjectType]): Unit = {
    val fieldNameMap = m.erasure.getMethods()
    		.filter(_.isAnnotationPresent(classOf[Key]))
    		.map {m => (m.getName(), m.getAnnotation(classOf[Key]).value())}
    		.toMap
    context.setFieldNameMap(fieldNameMap)
    val enhancer = new Enhancer()
    enhancer.setSuperclass(m.erasure)
    enhancer.setCallback(context)
    val constr = m.erasure.getConstructors()(0)
    val args: Array[Object] = constr.getParameterTypes().map(x => if(x.isPrimitive()) java.lang.reflect.Array.get(java.lang.reflect.Array.newInstance(x, 1), 0).asInstanceOf[Object] else null)
    val p = enhancer.create(constr.getParameterTypes(), args).asInstanceOf[ObjectType]
    val uc = uo(p)
    val udbo = uc.toDBObject(context)
    update(q, udbo, upsert, multi, wc)
  }
}
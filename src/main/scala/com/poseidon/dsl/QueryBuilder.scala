package com.poseidon.dsl

import com.poseidon.util._
import com.mongodb.DBObject
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.BasicDBObject
import java.util.regex.Pattern
import com.mongodb.BasicDBList

trait QueryClause {
  def toDBObject(c: Context): DBObject
  def &&(q: QueryClause) = new AndQueryClause(this, q)
  def ||(q: QueryClause) = new OrQueryClause(this, q)
}

class AndQueryClause(l: QueryClause, r: QueryClause) extends QueryClause {
  def toDBObject(c: Context): DBObject = {
    val dbo = new BasicDBObject()
    dbo.putAll(l.toDBObject(c))
    dbo.putAll(r.toDBObject(c))
    dbo
  }
}

class OrQueryClause(l: QueryClause, r: QueryClause) extends QueryClause {
  def toDBObject(c: Context): DBObject = {
    val dbo = new BasicDBObject()
    val cs = new BasicDBList
    cs.add(l.toDBObject(c))
    cs.add(r.toDBObject(c))
    MongoDBObject("$or" -> cs)
  }
}

class TermQueryClause[V](fieldRef: => V, op: CondOps.Value, v: V) extends QueryClause {
  def toDBObject(c: Context): DBObject = {
    fieldRef
    val fieldName = c.takeFieldName()
    MongoDBObject(fieldName -> MongoDBObject(op.toString() -> v))
  }
}

class ListQueryClause[V, L <% java.util.List[V]](fieldRef: => V, op: CondOps.Value, vs: L) extends QueryClause {
  def toDBObject(c: Context): DBObject = {
    fieldRef
    val fieldName = c.takeFieldName()
    MongoDBObject(fieldName -> MongoDBObject(op.toString() -> vs))
  }
}

class EqClause[V](fieldRef: => V, v: V) extends QueryClause {
  def toDBObject(c: Context): DBObject = {
    fieldRef
    val fieldName = c.takeFieldName()
    MongoDBObject(fieldName -> v)
  }
}

class EmptyQueryClause[V](fieldRef: => V) extends QueryClause {
  def toDBObject(c: Context): DBObject = new BasicDBObject
}

class QueryField[V](fieldRef: => V) {
  def $eqs(v: V) = new EqClause[V](fieldRef, v)
  def $neqs(v: V) = new TermQueryClause(fieldRef, CondOps.Ne, v)
  def $in[L <% List[V]](vs: L) = QueryHelpers.inListClause(fieldRef, vs)
  def $nin[L <% List[V]](vs: L) = new TermQueryClause(fieldRef, CondOps.Nin, QueryHelpers.list(vs))
  def $exists(b: Boolean) = new TermQueryClause(fieldRef, CondOps.Exists, b)
}

class NumericQueryField[V](fieldRef: => V) {
  def $lt(v: V) = new TermQueryClause(fieldRef, CondOps.Lt, v)
  def $gt(v: V) = new TermQueryClause(fieldRef, CondOps.Gt, v)
  def $lte(v: V) = new TermQueryClause(fieldRef, CondOps.LtEq, v)
  def $gte(v: V) = new TermQueryClause(fieldRef, CondOps.GtEq, v)
}

class ListQueryField[V, L <% List[V]](fieldRef: => L) {
  def $all(vs: Iterable[V]) = QueryHelpers.allListClause(fieldRef, vs)
  def $size(s: Int) = new TermQueryClause(fieldRef, CondOps.Size, s)
  def $contains(v: V) = new EqClause(fieldRef, v)
  def $notcontains(v: V) = new TermQueryClause(fieldRef, CondOps.Ne, v)
}

//////////////////////////////////////////////////////////////////////////////////
/// Modify fields
//////////////////////////////////////////////////////////////////////////////////

trait ModifyClause {
  def toDBObject(c: Context): DBObject
  def ~(mc: ModifyClause): ModifyClauseList = new ModifyClauseList(mc, this)
}

class ModifyClauseList(mc1: ModifyClause, mc2: ModifyClause) extends ModifyClause {
  var mcs = List(mc1, mc2)
  
  def toDBObject(c: Context): DBObject = {
    val dbo = new BasicDBObject
    for (mc <- mcs) dbo.putAll(mc.toDBObject(c))
    dbo
  }
  
  override def ~(mc: ModifyClause) = {
    mcs = mc :: mcs
    this
  }
}

class TermModifyClause[V](fieldRef: => V, op: ModOps.Value, v: V) extends ModifyClause {
  def toDBObject(c: Context): DBObject = {
    fieldRef
    val fieldName = c.takeFieldName()
    MongoDBObject(op.toString() -> MongoDBObject(fieldName -> v))
  }
}

class ModifyField[V](fieldRef: => V) {
  def $setTo(v: V) = new TermModifyClause(fieldRef, ModOps.Set, v)
  def $unset = new TermModifyClause(fieldRef, ModOps.Unset, 1)
}

class NumericModifyField[V](fieldRef: => V) {
  def $inc(v: V) = new TermModifyClause(fieldRef, ModOps.Inc, v)
}

class ListModifyField[V, L <% List[V]](fieldRef: => L) {
  def $setTo(vs: List[V]) = new TermModifyClause(fieldRef, ModOps.Set, vs)
  def $push(v: V) = new TermModifyClause(fieldRef, ModOps.Push, v)
  def $pushAll(vs: List[V]) = new TermModifyClause(fieldRef, ModOps.PushAll, QueryHelpers.list(vs))
  def $addToSet(v: V) = new TermModifyClause(fieldRef, ModOps.AddToSet, v)
  def $popFirst = new TermModifyClause(fieldRef, ModOps.Pop, -1)
  def $popLast = new TermModifyClause(fieldRef, ModOps.Pop, 1)
  def $pull(v: V) = new TermModifyClause(fieldRef, ModOps.Pull, v)
  def $pullAll(vs: List[V]) = new TermModifyClause(fieldRef, ModOps.PullAll, QueryHelpers.list(vs))
}

object QueryBuilder {
  implicit def intFieldToQueryField(fieldRef: => Int) = new NumericQueryField[Int](fieldRef)
  implicit def longFieldToQueryField(fieldRef: => Long) = new NumericQueryField[Long](fieldRef)
  implicit def doubleFieldToQueryField(fieldRef: => Double) = new NumericQueryField[Double](fieldRef)
  implicit def listFieldToQueryField[V](fieldRef: => List[V]) = new ListQueryField[V, List[V]](fieldRef)
  implicit def fieldToQueryField[V](fieldRef: => V) = new QueryField[V](fieldRef)
  
  implicit def intFieldToModifyField(fieldRef: => Int) = new NumericModifyField[Int](fieldRef)
  implicit def longFieldToModifyField(fieldRef: => Long) = new NumericModifyField[Long](fieldRef)
  implicit def doubleFieldToModifyField(fieldRef: => Double) = new NumericModifyField[Double](fieldRef)
  implicit def listFieldToModifyField[V](fieldRef: => List[V]) = new ListModifyField[V, List[V]](fieldRef)
  implicit def fieldToModifyField[V](fieldRef: => V) = new ModifyField[V](fieldRef)
}
package com.poseidon.dsl

import com.mongodb.DBObject
import com.poseidon.util.CondOps

object QueryHelpers {
  def makeJavaList[T](sl: Iterable[T]): java.util.List[T] = {
    val list = new java.util.ArrayList[T]()
    for (id <- sl) list.add(id)
    list
  }

  def list[T](vs: Iterable[T]): java.util.List[T] = {
    makeJavaList(vs)
  }

  def list(vs: Double*): java.util.List[Double] = list(vs)

  def makeJavaMap[K, V](m: Map[K, V]): java.util.Map[K, V] = {
    val map = new java.util.HashMap[K, V]
    for ((k, v) <- m) map.put(k, v)
    map
  }

  def inListClause[V](fieldRef: => V, vs: Iterable[V]) = {
    if (vs.isEmpty)
      new EmptyQueryClause[V](fieldRef)
    else
      new ListQueryClause(fieldRef, CondOps.In, list(vs))
  }

  def allListClause[V](fieldRef: => V, vs: Iterable[V]) = {
    if (vs.isEmpty)
      new EmptyQueryClause[V](fieldRef)
    else
      new ListQueryClause(fieldRef, CondOps.All, list(vs))
  }
}

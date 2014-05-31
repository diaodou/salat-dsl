package com.matrix.salat

import com.matrix.salat.dsl._
import com.novus.salat._

object Imports {
  type BaseDao[ObjectType <: CaseClass, ID <: Any] = com.matrix.salat.dsl.BaseDao[ObjectType, ID]

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
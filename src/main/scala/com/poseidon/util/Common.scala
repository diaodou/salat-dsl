package com.poseidon.util

import java.lang.reflect.Method
import net.sf.cglib.proxy.MethodInterceptor
import net.sf.cglib.proxy.MethodProxy

object CondOps extends Enumeration(0, "$ne", "$lt", "$gt", "$lte", "$gte", "$in", "$nin", "$near", "$all", "$size", "$exists", "$type", "$mod") {
  type Op = Value
  val Ne, Lt, Gt, LtEq, GtEq, In, Nin, Near, All, Size, Exists, Type, Mod = Value
}

object ModOps extends Enumeration(0, "$inc", "$set", "$unset", "$push", "$pushAll", "$addToSet", "$pop", "$pull", "$pullAll") {
  type Op = Value
  val Inc, Set, Unset, Push, PushAll, AddToSet, Pop, Pull, PullAll = Value
}

object MongoType extends Enumeration {
  type MongoType = Value
  val Double = Value(1)
  val String = Value(2)
  val Object = Value(3)
  val Array = Value(4)
  val Binary = Value(5)
  val ObjectId = Value(7)
  val Boolean = Value(8)
  val Date = Value(9)
  val Null = Value(10)
  val RegEx = Value(11)
  val JavaScript = Value(13)
  val Symbol = Value(15)
  val Int32 = Value(16)
  val Timestamp = Value(17)
  val Int64 = Value(18)
  val MaxKey = Value(127)
  val MinKey = Value(255)
}

class Context extends  MethodInterceptor {
  var name: String = null
  var fieldNameMap = Map[String, String]()
  
  def takeFieldName(): String = {
    val f = name
    name = null
    f
  }
  
  def setFieldNameMap(map: Map[String, String]) = {
    fieldNameMap = map
  }
  
  override def intercept(obj: Object, method: Method, args: Array[Object], proxy: MethodProxy): Object = {
    name = method.getName()
    name = fieldNameMap.get(name).getOrElse(name)
    null
  }
}
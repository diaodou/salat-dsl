package com.matrix.salat.util

import com.novus.salat._
import java.lang.reflect.Method
import net.sf.cglib.proxy.MethodInterceptor
import net.sf.cglib.proxy.MethodProxy
import com.novus.salat.annotations.raw.Key
import net.sf.cglib.proxy.Enhancer

object CondOps extends Enumeration(0, "$ne", "$lt", "$gt", "$lte", "$gte", "$regex", "$in", "$nin", "$near", "$all", "$size", "$exists", "$type", "$mod") {
  type Op = Value
  val Ne, Lt, Gt, LtEq, GtEq, Regex, In, Nin, Near, All, Size, Exists, Type, Mod = Value
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

class ThreadLocal[T](init: => T) extends java.lang.ThreadLocal[T]{
  override def initialValue:T = init
}

class Context(val fieldNameMap: Map[String, String]) extends  MethodInterceptor {
  var name = new ThreadLocal[String](null)
  
  def takeFieldName(): String = {
    val f = name.get()
    name.set(null)
    f
  }
  
  override def intercept(obj: Object, method: Method, args: Array[Object], proxy: MethodProxy): Object = {
    val methodName = method.getName()
    val fieldName = fieldNameMap.get(methodName).getOrElse(methodName)
    name.set(if (name.get() == null) fieldName else name.get() + "." + fieldName)
    null
  }
}

object Context {
  val cache = scala.collection.mutable.Map[Class[_], (Context, CaseClass)]()
  
  def create[ObjectType <: CaseClass](clazz: Class[_]): (Context, ObjectType) = {
    val (c, o) = cache.getOrElseUpdate(clazz, createInner(clazz))
    (c, o.asInstanceOf[ObjectType])
  }
  
  def createInner[ObjectType <: CaseClass](clazz: Class[_]): (Context, ObjectType) = {
    val fieldNameMap = clazz.getMethods()
    		.filter(_.isAnnotationPresent(classOf[Key]))
    		.map {m => (m.getName(), m.getAnnotation(classOf[Key]).value())}
    		.toMap
    val context = new Context(fieldNameMap)
    val co = (context, createInner(context, clazz))
    co
  }
  
  def createInner[ObjectType <: CaseClass](c: Context, clazz: Class[_]): ObjectType = {
    val enhancer = new Enhancer()
    enhancer.setSuperclass(clazz)
    enhancer.setCallback(c)
    val constr = clazz.getConstructors()(0)
    val args: Array[Object] = constr.getParameterTypes().map(x => if(x.isPrimitive()) java.lang.reflect.Array.get(java.lang.reflect.Array.newInstance(x, 1), 0).asInstanceOf[Object] else null)
    val o = enhancer.create(constr.getParameterTypes(), args).asInstanceOf[ObjectType]
    o
  }
}
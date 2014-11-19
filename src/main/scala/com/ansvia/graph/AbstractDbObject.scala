package com.ansvia.graph

import com.tinkerpop.blueprints._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

trait AbstractDbObject[T] {
    def getVertex:Vertex
    def save()(implicit db:Graph):Vertex
    def delete()(implicit db:Graph)
    def isSaved:Boolean
    def reload[T: ClassTag: TypeTag]()(implicit db: Graph):this.type
    def -->(label:String)(implicit db:Graph):EdgeWrapper

}

package com.github.nishi_7
package front

sealed abstract class Type {
  val nullable: Boolean = false
}

object Type {
  case object Any extends Type {
    override val nullable: Boolean = true
    override def toString: String = "Any"
  }

  case object Void extends Type {
    override def toString: String = "Void"
  }

  case object Int extends Type {
    override def toString: String = "Int"
  }

  case object Char extends Type {
    override def toString: String = "Char"
  }

  case object Boolean extends Type {
    override def toString: String = "Boolean"
  }

  case object Null extends Type {
    override val nullable: Boolean = true
    override def toString: String = "Null"
  }

  case class ClassRef(name: String) extends Type {
    override val nullable: Boolean = true
    override def toString: String = name
  }
}

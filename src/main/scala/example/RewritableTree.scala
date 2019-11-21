package example


trait RewritableTree[T] {
  def children(node: T): List[T]

  def replaceChildren(node: T, newChildren: List[T]): T


}

sealed trait Data


case class Data1(str: String, list: List[Data]) extends Data

case class Data2(str: String, list: List[Data]) extends Data


object RewritableTree {

  implicit class RewritableTreeOps[T: RewritableTree](node: T) {

    def children(): List[T] = implicitly[RewritableTree[T]].children(node)

    def replaceChildren(newChildren: List[T]): T = implicitly[RewritableTree[T]].replaceChildren(node, newChildren)

  }


  implicit object DataRewritableTree extends RewritableTree[Data] {
    def children(node: Data): List[Data] = node match {
      case Data1(str) =>
      case Data2(str) =>

    }


  }


  def rewrite[T: RewritableTree](f: PartialFunction[T, T]): T => T = t => {
    rewrite0(f)(t).getOrElse(t)
  }

  private def rewrite0[T: RewritableTree](f: PartialFunction[T, T])(t: T): Option[T] = {
    import RewritableTree.ops._ // импортируем "методы", сгенерированные simulacrum'ом
    val rt = implicitly[RewritableTree[T]]
    "метода"
    val children = t.children // rt.children(t)

    var changed = false // кроме собственно переписывания, нам надо знать, произошло ли изменение, чтобы не переписывать всё дерево без надобности
    val updatedChildren = children.map { child =>
      val res = rewrite0(f)(child)
      changed = changed || res.isDefined
      res.getOrElse(child)
    }
    // альтернативная реализация без локальной изменяемой переменной
    //def rewriteList(lst: List[T], result: mutable.ListBuffer[T], changed: Boolean): (List[T], Boolean) = lst match {
    //  case Nil => (result.toList, changed)
    //  case head :: tail =>
    //    val res = rewrite0(f)(head)
    //    rewriteList(tail, result.append(res.getOrElse(head)), changed || res.isDefined)
    //}
    //val (updatedChildren, changed) = rewriteList(t.children, mutable.ListBuffer(), false)
    val updatedTree = if (changed)
      t.replaceChildren(updatedChildren)
    else
      t
    var changed2 = true
    val updatedTree2 = f.applyOrElse(t1, (_: T) => {
      changed2 = false;
      updatedTree
    })
    if (changed || changed2)
      Some(updatedTree2)
    else
      None
  }
}
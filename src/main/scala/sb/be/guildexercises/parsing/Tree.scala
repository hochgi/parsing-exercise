package sb.be.guildexercises.parsing

import scala.annotation.tailrec
import scala.util.matching.Regex

final case class Tree(value: String, children: List[Tree])
object Tree {

  def asIterator(tree: Tree): Iterator[(String,Int)] = new Iterator[(String,Int)] {

    private[this] var internal: Iterator[(String,Int)] = Iterator.single(tree.value -> tree.children.length)
    private[this] var remaining: List[Tree] = tree.children

    override def hasNext: Boolean = internal.hasNext || remaining.nonEmpty

    override def next(): (String, Int) = {
      if(internal.hasNext) internal.next()
      else {
        remaining.headOption.foreach { tree =>
          internal = asIterator(tree)
          remaining = remaining.tail
        }
        internal.next()
      }
    }
  }

  def format(tree: Tree): String = asIterator(tree).map {
    case (value, numberOfChildren) => s"$value:$numberOfChildren"
  }.mkString(",")

  val SingleNode: Regex =  "([a-zA-Z0-9]*):(\\d*)".r
  def parse(input: String): Option[Tree] = input.split(',').foldLeft(List.empty[(Tree,Int)]){
    case ((Tree(v0,cs), i) :: tail, SingleNode(v1, "0")) => collapse((Tree(v0, Tree(v1, Nil) :: cs), math.max(0, i - 1)) :: tail)
    case (list,                     SingleNode(vl, num)) => (Tree(vl, Nil), Integer.parseInt(num)) :: list
  }.headOption.map { case (tree, _) =>
    tree.copy(children = tree.children.reverse)
  }

  @tailrec def collapse(ts: List[(Tree,Int)]): List[(Tree,Int)] = ts match {
    case (Tree(v0,c0),0) :: (Tree(v1,c1), i) :: tail => collapse((Tree(v1, Tree(v0, c0.reverse) :: c1), i - 1) :: tail)
    case doNotChange                                 => doNotChange
  }
}

package sb.be.guildexercises.parsing

import fastparse._, NoWhitespace._
import fastparse.Parsed.{Success, Failure}

final case class Tree(value: String, children: List[Tree])
object Tree {

  def format(tree: Tree): String = {
    val sb = new StringBuilder
    def inner(subTree: Tree): Unit = {
      sb ++= subTree.value
      var ch = '('
      subTree.children.foreach { childNode =>
        sb += ch
        ch = ','
        inner(childNode)
      }
      if(ch == ',') sb += ')'
    }
    inner(tree)
    sb.result()
  }

  def value[_ : P]:    P[String]     = P(CharsWhile(_.isLetterOrDigit).!)
  def children[_ : P]: P[List[Tree]] = P(NoTrace("(") ~ tree.rep(1,",") ~ NoTrace(")")).map(_.toList)
  def tree[_ : P]:     P[Tree]       = P((value ~ children).map{ case (v,ts) => Tree(v,ts) } | value.map(Tree(_, Nil)))

  def parse(input: String): Option[Tree] = fastparse.parse(input, tree(_)) match {
    case Success(t, _) => Some(t)
    case _             => None
  }
}

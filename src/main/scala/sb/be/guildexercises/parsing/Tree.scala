package sb.be.guildexercises.parsing

import scala.util.parsing.combinator._

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

  object TreeParser extends RegexParsers {
    def value:    Parser[String]     = "[A-Za-z0-9]*".r
    def children: Parser[List[Tree]] = "(" ~> rep1sep(tree, ",") <~ ")"
    def tree:     Parser[Tree]       = (value ~ children) ^^ { case v ~ ts => Tree(v,ts) } | value ^^ (Tree(_,Nil))

    def parse(input: String): Option[Tree] = parseAll[Tree](tree, input) match {
      case Success(tree, _) => Some(tree)
      case _                => None
    }
  }

  def parse(input: String): Option[Tree] = TreeParser.parse(input)
}

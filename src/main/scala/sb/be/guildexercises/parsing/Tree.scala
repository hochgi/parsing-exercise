package sb.be.guildexercises.parsing

import zio.Chunk
import zio.parser._

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
  def parse(input: String): Option[Tree] = tree.parseString(input).toOption

  val value: Syntax[String, Char, Char, String] = Syntax.alphaNumeric.*.transform(_.mkString, s => Chunk.fromArray(s.toCharArray))
  val delim: Syntax[String, Char, Char, Unit] = Syntax.char(',')
  val children: Syntax[String, Char, Char, Chunk[Tree]] = Syntax.char('(') ~> tree.repeatWithSep(delim) <~ Syntax.char(')')
  val tree: Syntax[String, Char, Char, Tree] = (value ~ children.?).transform[Tree](
    { case (nodeValue, maybeChildren) => Tree(nodeValue, maybeChildren.fold(List.empty[Tree])(_.toList)) },
    { case Tree (nodeValue, children) => nodeValue -> Option.when(children.nonEmpty)(Chunk.fromIterable(children)) }
  )
}

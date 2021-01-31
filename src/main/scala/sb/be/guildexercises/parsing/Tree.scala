package sb.be.guildexercises.parsing

import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.jdk.CollectionConverters._
import scala.util.Try

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

  def parse(input: String): Option[Tree] = Try {
    val lexer = new SBTreeLexer(CharStreams.fromString(input))
    val tokens = new CommonTokenStream(lexer)
    val parser = new SBTreeParser(tokens)
    buildTree(parser.sbtree())
  }.toOption

  def buildTree(ctx: SBTreeParser.SbtreeContext): Tree = {
    val node: TerminalNode = ctx.VALUE()
    val value: String = node.getText
    val children = ctx.sbtree().asScala.toList.map(buildTree)
    Tree(value, children)
  }
}

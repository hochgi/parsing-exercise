package sb.be.guildexercises.parsing

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
  def parse(input: String): Option[Tree] = ???
}

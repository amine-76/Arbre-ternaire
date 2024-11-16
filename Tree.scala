sealed trait Tree[+A] {
    // méthode de réprentation de l'arbre 
      def toBranchString(indent: String = ""): String = this match {
        case Leaf => indent + "└── Leaf\n"
        case Node(value, char, left, next, right) =>
            val valueStr = value.map(v => s"(${v})").getOrElse("")
            val currentNode = s"$indent└── $char$valueStr\n"
            val leftStr = left.toBranchString(indent + "   ")
            val nextStr = next.toBranchString(indent + "   ")
            val rightStr = right.toBranchString(indent + "   ")
            currentNode + leftStr + nextStr + rightStr
    }
    // Utiliser Tree.insert directement
    def insert[B >: A](key: String, value: B): Tree[B] = Tree.insert(this, key, value, 0)
}

case object Leaf extends Tree[Nothing]

case class Node[A](
    value: Option[A],
    char: Char,
    left: Tree[A],
    next: Tree[A],
    right: Tree[A]
) extends Tree[A]

object Tree {

    def apply[A](): Tree[A] = Leaf

    def insert[A](root: Tree[A], key: String, value: A, n: Int): Tree[A] = root match {
        case Leaf => insert(Node(None, key.charAt(n), Leaf, Leaf, Leaf), key, value, n)
        case node: Node[A] if (node.char > key.charAt(n)) =>
            node.copy(left = insert(node.left, key, value, n))
        case node: Node[A] if (node.char < key.charAt(n)) =>
            node.copy(right = insert(node.right, key, value, n))
        case node: Node[A] if (n < key.length - 1) =>
            node.copy(next = insert(node.next, key, value, n + 1))
        case node: Node[A] =>
            node.copy(value = Some(value))
    }
}

object TestTree {
    def main(args: Array[String]): Unit = {
        val tree = Tree[Boolean]()
            .insert("chien", true)
            .insert("chat", true)
            .insert("coq", true)
            .insert("pie", true)
        println(tree.toBranchString())
    }
}

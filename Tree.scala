sealed trait Tree[+A] {
    // méthode de réprentation de l'arbre 
      def toTreeString(prefix: String = "", isTail: Boolean = true, branchType: String = ""): String = this match {
        case Leaf => prefix + (if (isTail) "└── " else "├── ") + s"Leaf $branchType\n"
        case Node(value, char, left, next, right) =>
        val valueStr = value.map(v => s"(${v})").getOrElse("")
        val currentNode = prefix + (if (isTail) "└── " else "├── ") + s"$char$valueStr $branchType\n"

        val leftStr = left.toTreeString(prefix + (if (isTail) "    " else "│   "), next == Leaf && right == Leaf, "[L]")
        val nextStr = next.toTreeString(prefix + (if (isTail) "    " else "│   "), right == Leaf, "[N]")
        val rightStr = right.toTreeString(prefix + (if (isTail) "    " else "│   "), isTail, "[R]")

        currentNode + leftStr + nextStr + rightStr
}
    // Utiliser Tree.insert directement
    def insert[B >: A](key: String, value: B): Tree[B] = Tree.insert(this, key, value, 0)

    // Méthode size()
    def size() : Int = this match {
        case Leaf => 0
        case Node(value,_,left,next,right) => 
            val currentNodeSize = if(value.isDefined) 1 else 0 
            currentNodeSize + left.size() + next.size + right.size()                                
    }
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
    def insert2(root : Tree[Boolean], key : String): Tree[Boolean] = insert(root, key, true , 0)
    
}

object TestTree {
    def main(args: Array[String]): Unit = {
        val tree = Tree[Boolean]()
            .insert("chien", true)
            .insert("chat", true)
            .insert("coq", true)
            .insert("pie", true)
            
        //println(tree.toTreeString())
        val test = Tree[Boolean]()
        val update = Tree.insert2(tree,"amine")
        val up2 = Tree.insert2(update, "toto")
        println(up2.toTreeString())

        // Test de la méthode size()
        val n = tree.size()
        println("Size : "+n) // affiche 4 ppur l'arbre de base 
    }
}

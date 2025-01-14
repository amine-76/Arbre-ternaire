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
    
    def toList() : List[A] = this match {
        case Leaf => Nil  
        case Node(value,_,left,next,right) => 
            val currentValue = value.toList
            currentValue ++ left.toList ++ next.toList ++ right.toList     
    }
    
    def get(key: String, n: Int = 0): Option[A] = this match {
        case Leaf => None // Si on atteint une feuille, la clé n'existe pas.
        case Node(value, char, left, next, right) =>
            if (key.charAt(n) < char) {
            left.get(key, n) // La lettre est avant le caractère courant, on cherche à gauche.
            } else if (key.charAt(n) > char) {
            right.get(key, n) // La lettre est après, on cherche à droite.
            } else if (n == key.length - 1) {
                value // La clé est complètement parcourue, on retourne la valeur.
            } else {
            next.get(key, n + 1) // La lettre correspond, on passe au caractère suivant via `next`.
            }
    }

    def contains(key : String, n : Int = 0) : Boolean = this match {
        case Leaf => false
        case Node(value, char, left, next, right) =>
            if (key.charAt(n) < char) {
                 left.contains(key, n) // La lettre est avant le caractère courant, on cherche à gauche.
            } else if (key.charAt(n) > char) {
                        right.contains(key, n) // La lettre est après, on cherche à droite.
            } else if (n == key.length - 1) {
                value.isDefined // La clé est complètement parcourue, on retourne la valeur.
            } else {
                next.contains(key, n + 1) // La lettre correspond, on passe au caractère suivant via `next`.
            }
    }
    def toKeyValueList(prefix : String = "") : List[(String,A)] = this match {
        case Leaf => Nil
        case Node(value, char, left, next, right) =>
          val currentKey = prefix + char

          val currentValue = value match {
            case Some(value) => List((currentKey,value))
            case None => Nil
          }  
            currentValue ++
            left.toKeyValueList(prefix) ++ 
            next.toKeyValueList(currentKey) ++ 
            right.toKeyValueList(prefix)   
    }
       
    def remove(key: String, n: Int = 0): Tree[A] = this match {
    case Leaf => Leaf // Si l'arbre est vide, rien à supprimer.

    case Node(value, char, left, next, right) =>
        if (key.charAt(n) < char) {
        // Mise à jour manuelle du sous-arbre gauche.
        Node(value, char, left.remove(key, n), next, right)
        } else if (key.charAt(n) > char) {
        // Mise à jour manuelle du sous-arbre droit.
        Node(value, char, left, next, right.remove(key, n))
        } else if (n == key.length - 1) {
        // Suppression de la valeur si la clé est trouvée.
        if (left == Leaf && next == Leaf && right == Leaf) {
            Leaf // Si toutes les branches sont mortes, on retourne une feuille.
        } else {
            // Sinon, recrée un nœud avec la valeur supprimée.
            Node(None, char, left, next, right)
        }
        } else {
        // Mise à jour manuelle du sous-arbre suivant.
        Node(value, char, left, next.remove(key, n + 1), right)
        }
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
        val up2 = Tree.insert2(update, "kifech")
        println(up2.toTreeString())

        // Test de la méthode size()
        val n = tree.size()
        println("Size : "+n) // affiche 4 ppur l'arbre de base 
        
        // Test Méthode toList
        val list = tree.toList()
        println("List : "+list)

        // Test méthode get() : 
        println(tree.get("chien")) // Résultat attendu : Some(true)
        println(tree.get("chat"))  // Résultat attendu : Some(true)
        println(tree.get("chaton")) // Résultat attendu : None (non inséré)
        println(tree.get("pie"))  // Résultat attendu : Some(true)
        println(tree.get("ami"))  // Résultat attendu : None (non inséré)
        
        // test méthode contains() : 
        println("contains chien ? "+tree.contains("chien")); 
        println("contains chat ? "+tree.contains("chat")); 
        println("contains chaton ? "+tree.contains("chaton")); 
        println("contains pie ? "+tree.contains("pie")); 
        println("contains ami ? "+tree.contains("ami")); 
        
        // test méthode toKeyValueList() 
        println("List de tuple (key,value) "+tree.toKeyValueList());

        // test méthode remove()
        val tree_remove1 =  tree.remove("chien")
        val tree_remove2 =  tree_remove1.remove("coq")
        val tree_remove3 =  tree_remove2.remove("pie")

        println("Après suppression de toutes les clés :")
        println(tree_remove3.toKeyValueList())
    }
}

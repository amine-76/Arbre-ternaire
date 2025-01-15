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
   def size() = Tree.size(this)


   // Méthode toList
   def toList() : List[A] = Tree.toList(this)


    // Méthode get
   def get(key: String): Option[A] = Tree.get(this,key)

    // Méthode contains 
   def contains(key : String) : Boolean = Tree.contains(this,key)
   
   // Méthode toKeyValueList

   def toKeyValueList(prefix : String = "") : List[(String,A)] = Tree.toKeyValueList(this,prefix)
   
   def remove(key: String): (Option[A],Tree[A]) = Tree.remove(this,key)

 

}

case object Leaf extends Tree[Nothing]

case class Node[A](
   value: Option[A],
   char: Char,
   left: Tree[A],
   next: Tree[A],
   right: Tree[A]
) extends Tree[A]

case object Tree {

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
   // 1
   def insert(root: Tree[Boolean], key: String): Tree[Boolean] = insert(root, key, true, 0)
   /*
Avec cet ensemble, nous n'obtiendrons jamais la valeur `false` car la méthode `insert` 
associe uniquement la valeur `true` à chaque clé insérée, indiquant sa présence dans l'ensemble. 
Si une clé non présente est demandée via des méthodes comme `get` ou `contains`, le résultat 
sera respectivement `None` ou `false`, ce qui reflète l'absence de cette clé dans l'ensemble. 
L'ensemble ne stocke que des clés explicitement insérées, sans possibilité de représenter 
des valeurs négatives ou absentes.
*/


   // 2
   def size[A](tree: Tree[A]): Int = tree match {
       case Leaf => 0
       case Node(value,_,left,next,right) =>
           val valeurCourant = if (value.isDefined) 1 else 0
           valeurCourant + size(left) + size(next) + size(right)
   }
   //3
   def toList[A](root : Tree[A]) : List[A] = root match {
       case Leaf => Nil  
       case Node(value,_,left,next,right) => 
           val currentValue = value.toList
           currentValue ++ left.toList ++ next.toList ++ right.toList     
   }
   // 4 
      def get[A](root: Tree[A],key: String, n: Int = 0): Option[A] = root match {
       case Leaf => None // Si on atteint une feuille, la clé n'existe pas.
       case Node(value, char, left, next, right) =>
           if (key.charAt(n) < char) {
              get(left,key, n) // La lettre est avant le caractère courant, on cherche à gauche.
           } else if (key.charAt(n) > char) {
              get(right,key, n) // La lettre est après, on cherche à droite.
           } else if (n == key.length - 1) {
               value // La clé est complètement parcourue, on retourne la valeur.
           } else {
               get(next,key, n + 1) // La lettre correspond, on passe au caractère suivant via `next`.
           }
        }

    //5 
    
     def contains[A](root : Tree[A],key : String, n : Int = 0) : Boolean = root match {
       case Leaf => false
       case Node(value, char, left, next, right) =>
           if (key.charAt(n) < char) {
                contains(left,key, n) // La lettre est avant le caractère courant, on cherche à gauche.
           } else if (key.charAt(n) > char) {
                contains(right,key, n) // La lettre est après, on cherche à droite.
           } else if (n == key.length - 1) {
               value.isDefined // La clé est complètement parcourue, on retourne la valeur.
           } else {
               contains(next,key, n + 1) // La lettre correspond, on passe au caractère suivant via `next`.
           }
        }  
    //6
     def toKeyValueList[A](root : Tree[A],prefix : String = "") : List[(String,A)] = root match {
       case Leaf => Nil
       case Node(value, char, left, next, right) =>
         val currentKey = prefix + char

         val currentValue = value match {
           case Some(value) => List((currentKey,value))
           case None => Nil
         }  
           currentValue ++
           toKeyValueList(left,prefix) ++ 
           toKeyValueList(next,currentKey) ++ 
           toKeyValueList(right,prefix)   
       }      
    //7
    def remove[A](root: Tree[A], key: String, n: Int = 0): (Option[A], Tree[A]) = {
    if (key.isEmpty) (None, root) // Gérer les clés vides.
    else root match {
        case Leaf =>
            (None, Leaf) // Si l'arbre est vide, rien à supprimer.

        case Node(value, char, left, next, right) =>
            if (key.charAt(n) < char) {
                val (removedValue, updatedLeft) = remove(left, key, n)
                (removedValue, Node(value, char, updatedLeft, next, right))
            } else if (key.charAt(n) > char) {
                val (removedValue, updatedRight) = remove(right, key, n)
                (removedValue, Node(value, char, left, next, updatedRight))
            } else if (n == key.length - 1) {
                // Si on arrive à la fin de la clé, supprimer la valeur associée.
                val newNode = if (left == Leaf && next == Leaf && right == Leaf) {
                    Leaf // Si toutes les branches sont mortes, retourner une feuille.
                } else {
                    Node(None, char, left, next, right) // Supprimer seulement la valeur.
                }
                (value, newNode)
            } else {
                val (removedValue, updatedNext) = remove(next, key, n + 1)
                val newNode = if (value.isEmpty && left == Leaf && updatedNext == Leaf && right == Leaf) {
                    Leaf // Si le nœud devient inutile, retourner une feuille.
                } else {
                    Node(value, char, left, updatedNext, right) // Sinon, garder le nœud.
                }
                (removedValue, newNode)
            }
            
        }
    }

/*
La méthode `remove` permet de supprimer un élément d'un arbre en fonction d'une clé donnée. Elle parcourt l'arbre en comparant les caractères de la clé avec les nœuds de l'arbre, se déplaçant à gauche, à droite ou dans le sous-arbre suivant selon la comparaison des caractères. Si la clé est trouvée et que l'on est à la fin de la chaîne de caractères (lorsque `n == key.length - 1`), la suppression est effectuée. Si toutes les branches (gauche, droite et suivant) sont mortes (c'est-à-dire des feuilles), l'arbre retourne une feuille vide. Si un nœud possède encore des sous-arbres valides, l'arbre est réorganisé pour maintenir ces sous-arbres, mais la valeur est supprimée en la remplaçant par `None`.

Concernant les questions ouvertes :

1. **L’élément supprimé est-il la seule valeur à retourner ?**

L’élément supprimé est effectivement la seule valeur explicitement retournée sous la forme d’un `Option[A]`. 
Cela permet de savoir si une valeur a été supprimée ou si la clé spécifiée n’existait pas dans l’arbre. 
Cependant, pour garantir l’intégrité de l’arbre, la méthode retourne également une version mise à jour de l’arbre (`Tree[A]`) après suppression. 
Ainsi, le tuple `(Option[A], Tree[A])` combine deux informations essentielles : 
1. La valeur supprimée (ou `None` si la clé n’existe pas). 
2. L’arbre modifié après la suppression. 
Cette approche assure que l’utilisateur peut récupérer l’élément supprimé tout en préservant la cohérence structurelle de l’arbre.

2. **Que faire si on fait deux ou trois suppressions de suite ?**
   Après chaque suppression, l'arbre est mis à jour. Si une clé est supprimée et n'existe plus dans l'arbre, toute tentative de suppression future pour cette même clé n'aura aucun effet, car l'arbre ne contient plus cette clé. L'arbre est réajusté à chaque suppression.

3. **Que faire si la valeur n’existe pas ?**
   Si la valeur n'existe pas dans l'arbre, la méthode ne modifie pas l'arbre et retourne simplement l'arbre inchangé avec une valeur "None" qui est la valeur de la clé inexistante . Cela est géré automatiquement par la manière dont l'arbre est traversé à chaque étape, sans trouver de correspondance pour la clé donnée.

4. **Peut-on supprimer des éléments qui ne sont pas en bas de l’arbre ?**
   Oui, la méthode peut supprimer des éléments situés à n'importe quel niveau de l'arbre. L'algorithme fonctionne en comparant les caractères de la clé avec les nœuds, donc l'élément à supprimer peut être n'importe où dans l'arbre, qu'il soit en bas ou non.

5. **Peut-il rester des lettres inutilisées dans un mot après suppression ?**
   Oui, après suppression d'une clé, il peut en effet rester une lettre inutiliser de cette clé dans le cas où cette clé à suprimmer à été ajouter en premier.
   En effet, le noeud possèdant la lettre inutiliser sera lié à un autre séquence de caractère menant à la valeur d'une autre clé. Si l'on supprimer ce noeud possédant la lettre inutiliser, on risque également de supprimer une branche menant vers une autre valeur d'une clé. 

   Prenons un exemple: 
   On décide d'ajouté la clé "chien" en premier ensuite "chat". En ajoutant "chat" la lettre "a" de "chat" sera le "gauche" de "i" de "chien" . 
   Ainsi, en supprimant la clé "chien", nous supprimons le "e" et le "n" mais pas le "i" car il est lié a une autre séquence de la clé chat. *
   Cela est dû au faite que l'on se passe de toute optimisation (réarrangement). 
*/

}

object TestTree {
   def main(args: Array[String]): Unit = {
       val tree = Tree[Boolean]()
           .insert("chien", true)
           .insert("chat", true)
           .insert("coq", true)
           .insert("pie", true)

           
       //println(tree.toTreeString())

      // Test insert question 1 
       val arbre1 = Tree.insert(tree,"kifech")
       val arbre2 = Tree.insert(tree,"chill")

       println(arbre2.toTreeString())

       // Test de la méthode size()
       val n = tree.size()
       println("Size : "+n) // affiche 4 pour l'arbre de base
       
       // Test Méthode toList
       val list = tree.toList()
       println("List : "+list) // (True, True, True, True)

       // Test méthode get() : 
       println(tree.get("chien")) // Résultat attendu : Some(true)
       println(tree.get("chat"))  // Résultat attendu : Some(true)
       println(tree.get("chaton")) // Résultat attendu : None (non inséré)
       println(tree.get("pie"))  // Résultat attendu : Some(true)
       println(tree.get("ami"))  // Résultat attendu : None (non inséré)
       
       // test méthode contains() : 
       println("contains chien ? "+tree.contains("chien")); //True 
       println("contains chat ? "+tree.contains("chat")); //True 
       println("contains chaton ? "+tree.contains("chaton")); //False
       println("contains pie ? "+tree.contains("pie")); // True 
       println("contains ami ? "+tree.contains("ami")); //false
       
       // test méthode toKeyValueList() 
       println("List de tuple (key,value) "+tree.toKeyValueList());//List de tuple (key,value) List((chat,true), (chien,true), (coq,true), (pie,true))

       // test méthode remove()
        val (removed1, tree_remove1) = tree.remove("chien")
       val (removed2, tree_remove2) = tree_remove1.remove("coq")
       val (removed3, tree_remove3) = tree_remove2.remove("pie")
       val (removed4, tree_remove4) = tree_remove3.remove("Kifech")

       println("Après suppression de toutes les clés :")
       println(tree_remove1.toTreeString())
       println(tree_remove3.toKeyValueList())
       println(tree_remove4.toKeyValueList())
   }
}
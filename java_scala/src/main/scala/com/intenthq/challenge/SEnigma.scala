package com.intenthq.challenge;

case class SuffixNode(number: Int, value: Option[Char] = None, children: Seq[SuffixNode] = Seq.empty[SuffixNode])

object SEnigma {

  // We have a system to transfer information from one place to another. This system
  // involves transferring only list of digits greater than 0 (1-9). In order to decipher
  // the message encoded in the list you need to have a dictionary that will allow
  // you to do it following a set of rules:
  //    > Sample incoming message: (​1,2,3,7,3,2,3,7,2,3,4,8,9,7,8)
  //    > Sample dictionary (​23->‘N’,234->‘ ’,89->‘H’,78->‘Q’,37 ->‘A’)
  //  - Iterating from left to right, we try to match sublists to entries of the map.
  //    A sublist is a sequence of one or more contiguous entries in the original list,
  //    eg. the sublist (1, 2) would match an entry with key 12, while the sublist (3, 2, 3)
  //    would match an entry with key 323.
  //  - Whenever a sublist matches an entry of the map, it’s replaced by the entry value.
  //    When that happens, the sublist is consumed, meaning that its elements can’t be used
  //    for another match. The elements of the mapping however, can be used as many times as needed.
  //  - If there are two possible sublist matches, starting at the same point, the longest one
  //    has priority, eg 234 would have priority over 23.
  //  - If a digit does not belong to any matching sublist, it’s output as is.
  //
  // Following the above rules, the message would be: “1N73N7 HQ”
  // Check the tests for some other (simpler) examples.

  def decipher(map: Map[Int, Char])(message: List[Int]): String = {
    def suffixTree(root: SuffixNode, token: Int, value: Char): SuffixNode = {
      def loop(current: SuffixNode, branch: Seq[Int]): SuffixNode = (current, branch) match {
        case (node, Seq(a)) if node.children.isEmpty => SuffixNode(node.number, node.value, Seq(SuffixNode(a, Some(value))))
        case (node, Seq(a)) if node.children.map(_.number).contains(a) => node
        case (node, Seq(a)) => SuffixNode(node.number, node.value, node.children ++ Seq(SuffixNode(a, Some(value))))
        case (node, x :: xs) if node.children.isEmpty => {
          val nextNode = SuffixNode(x, children = Seq(loop(nextNode, xs)))
          SuffixNode(node.number, node.value, Seq(nextNode))
        }
        case (node, x :: xs) if node.children.map(_.number).contains(x) => loop(node.children.filter(_.number == x).head, xs)
        case (node, x :: xs) => {
          val nextNode = SuffixNode(x)
          SuffixNode(node.number, node.value, node.children ++ Seq(nextNode, loop(nextNode, xs)))
        }
        case (node, _) => node
      }
      loop(root,token.toString.map(_.asDigit))
    }

    def search(tree: SuffixNode, message: List[Int]): String = {
      val strBuilder = new StringBuilder
      def step(current: SuffixNode, substring: List[Int]): Unit = (current, substring) match {
        case (node, List(a)) if (node.number == 0) && node.children.isEmpty => strBuilder.append(a)
        case (node, List(a)) if (node.number == 0) => node.children.map(step(_, List(a)))
        case (node, List(a)) if (node.number != a) && node.children.isEmpty => strBuilder.append(a)
        case (node, List(a)) if (node.number != a) => strBuilder.append(a)
        case (node, List(a)) => node.value.map(strBuilder.append(_))
        case (node, x :: xs) if (node.number == 0) && node.children.isEmpty => {
          strBuilder.append(x)
          step(node, xs)
        }
        case (node, x :: xs) if (node.number == 0) && node.children.isEmpty => {
          node.children.map(step(_, x :: xs))
        }
        case (node, x :: xs) if (node.number != x) && node.children.isEmpty => {
          strBuilder.append(x)
          step(node, xs)
        }
        case (node, x :: xs) if (node.number != x) => {
          node.children.map(step(_, x :: xs))
        }
        case (node, x :: xs) => node.children.map(step(_, xs))
      }
      step(tree, message)
      strBuilder.toString()
    }

    val root = SuffixNode(0)
    val tree = map.foldLeft(root){ case (root, (number, char)) => suffixTree(root, number, char)}

    search(tree, message)

    //val suffixes = map.values.map(_)
    /*
    "(2,3) is 'N'" in {
      deciphe(List(2,3)) must_== "N"
    }
    "(2,3,8,9) is 'NH'" in {
      deciphe(List(2,3,8,9)) must_== "NH"
    }

    "(1,2,3) is '1N'" in {
      deciphe(List(1,2,3)) must_== "1N"
    }*/
  }

}
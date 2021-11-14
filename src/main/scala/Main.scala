@main def hello: Unit =

    def sort(tree: SearchTree[Int]): List[Int] = {


      def sorting(tree: SearchTree[Int], lst: List[Int]): List[Int] = tree match {

        case Empty => List.empty[Int]

        case Node(Empty,y,Empty) => y::lst
        case Node(Empty,y,rgt) => y::sorting(rgt,lst)
        case Node(lft,y,Empty) => sorting(lft,y::lst)
        case Node(lft@Node(a,x,b),y,rgt@Node(c,z,d)) => sorting(lft,y::sorting(rgt,lst))
      }
      sorting(tree,List.empty[Int])
    }
    test("sort", sort _, "tree")

    def parseTree(str: String): SearchTree[Int] = {
      def parcer(stI:Int): (SearchTree[Int],Int) = {
        if (str(stI) == 'N' || str(stI) == 'L') (Node(parcer(1+digitParce(str,stI+1,"")._2)._1 , digitParce(str,stI+1,"")._1, parcer(2+digitParce(str,stI+1,"")._2)._1), parcer(1+digitParce(str,stI+1,"")._2)._2)
        else if (str(stI) == 'E') (Empty,stI)
      }

      def digitParce(str:String,stI:Int, numStr:String):(Int,Int) = {
        if (str(stI).isDigit) digitParce(str,stI+1,str(stI)+numStr)
        else (numStr.toString.toInt,stI)
      }

      parcer(0)
    }


    test("parseTree", parseTree _, "str")

    import eecs.regchecker._

    ignorecheckNoCursingWarmup {
      "not yet implemented"
    }

    ignorecheckNoCursing {
      "not yet implemented"
    }

    import eecs.cfgchecker._

    ignorecheck("Balancing Act") {
  """
  NOT = yet implemented;
  """
    }

  }

  /*  def parseTree(str: String): SearchTree[Int] = {
      def findDigits(str: String, i: Int,iEnd: Int,iStart:Int):String = {
        if (str(i+1).isDigit) findDigits(str,i,iEnd+1,iStart)
        else {
          if (iEnd == iStart) findDigits(str,i,iEnd-1,iStart)
          else str(iEnd-(iEnd-iStart))+findDigits(str,i,iEnd-1,iStart)
        }
      }
      def findElem = ???
    } */

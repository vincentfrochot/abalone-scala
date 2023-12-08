object AbaloneEngine {
  case class Pos(index: Int)              extends AnyVal {}
  sealed abstract class Color
  case object B                           extends Color  { override def toString = "B"              }
  case object W                           extends Color  { override def toString = "W"              }
  sealed abstract class Square            extends Product with Serializable
  final case class Occupied(color: Color) extends Square { override def toString = color.toString() }
  case object Empty                       extends Square { override def toString = "-"              }
//  case object Padding                     extends Square { override def toString = "x"              } // legacy - old system using squaredHexagon
//  case object Null                        extends Square { override def toString = "."              } // legacy - old system using squaredHexagon

  sealed class Piece(val player: Color) {
    def is(c: Color)    = c == player
    def isNot(c: Color) = c != player

    override def toString = s"$player".toLowerCase
  }

  case class Situation(val board: Board, val player: Color) {}

  case class Board(
      width: Int,
      score: (Int, Int),
      pieces: scala.collection.mutable.Map[Pos, Piece], // @TODO: use immutable (default)
      turn: Color = B
  ) {

    def apply(at: Pos): Option[Piece] = pieces get at
    // def apply(file: File, rank: Rank) = pieces get Pos(file, rank)

    override def toString() = {
      // @TODO: adapt this to avoid using a loop or at least make it less clunky
      var i = 0
      var j = 0
      while (i < this.width) {
        (0 until (this.width - i)).foreach(_ => print(" "))
        j = 0
        while (j < this.width + i) {
          if (this.pieces.exists(_._1 == Pos(Board.flattenCoordinates2D((i, j), this.width)))) {
            print(this.pieces(Pos(Board.flattenCoordinates2D((i, j), this.width))))
            print(" ") // @TODO: understand why I could not write + " " on the line just above.
          } else {
            print("- ")
          }
          j = j + 1
        }
        println("")
        i = i + 1
      }

      i = 0
      while (i < this.width - 1) {
        (0 until i + 1).foreach(_ => print(" "))
        j = 0
        print(" ")
        while (j < 2 * this.width - i - 2) {
          if (this.pieces.exists(_._1 == Pos(Board.flattenCoordinates2D((this.width + i, j), this.width)))) {
            print(this.pieces(Pos(Board.flattenCoordinates2D((this.width + i, j), this.width))))
            print(" ") // @TODO: understand why I could not write + " " on the line just above.
          } else {
            print("- ")
          }
          j = j + 1
        }
        println("")
        i = i + 1
      }
      ""
      // pieces.toString
    }
  }

  object Board {
//
//                i -> W W - B B                  00  01  02  03  04
//               h -> W W W B B B               05  06  07  08  09  10
//              g -> - W W - B B -            11  12  13  14  15  16  17
//             f -> - - - - - - - -         18  19  20  21  22  23  24  25
//            e -> - - - - - - - - -      26  27  28  29  30  31  32  33  34
//             d -> - - - - - - - - \       35  36  37  38  39  40  41  42
//              c -> - B B - W W - \ 9        43  44  45  46  47  48  49
//               b -> B B B W W W \ 8           50  51  52  53  54  55
//                a -> B B - W W \ 7              56  57  58  59  60
//                      \ \ \ \ \ 6
//                      1 2 3 4  5
//
//
// 0 0 ww1bb/wwwbbb/1ww1bb1/8/9/8/1ww1bb1/wwwbbb/ww1bb b       <- last b stands for "black to play"
    def fromFEN(fen: String): Board = {
      val subparts: Array[String] = fen.split(" ")
      val score                   = (subparts(0).toInt, subparts(1).toInt)
      val position                = subparts(2).split("/")
      val width                   = position.length / 2 + 1
      val turn                    = if (subparts(3).equals("w")) AbaloneEngine.W else AbaloneEngine.B

      // @TODO: I need to figure out how to use this position val efficiently to generate a list of marbles :
      // this code is pretty bad in Scala, we are not supposed to use loops.
      val blackPiecesMap: scala.collection.mutable.Map[Pos, Piece] = scala.collection.mutable.Map.empty
      val whitePiecesMap: scala.collection.mutable.Map[Pos, Piece] = scala.collection.mutable.Map.empty
      var currentIndex                                             = 0
      var i                                                        = 0
      var j                                                        = 0
      while (i < position.length) {
        j = 0
        while (j < position(i).length) {
          if (position(i)(j).isDigit) {
            if (j < position(i).length - 1 && position(i)(j + 1).isDigit) { // up to 99...
              currentIndex = currentIndex + (position(i)(j) + "" + position(i)(j + 1)).toInt
              j = j + 1
            } else {
              currentIndex = currentIndex + position(i)(j).asDigit
            }
          } else {
            currentIndex = currentIndex + 1
            if (position(i)(j).equals('b')) {
              blackPiecesMap(Pos(currentIndex - 1)) = new Piece(AbaloneEngine.B)
            } else {
              whitePiecesMap(Pos(currentIndex - 1)) = new Piece(AbaloneEngine.W)
            }
          }
          j = j + 1
        }
        i = i + 1
      }

      new Board(width, score, blackPiecesMap ++ whitePiecesMap, turn)
    }

    /*
      To check an Abalone move, we need to consider :
        - the distance between orig and dest
        - the accumulated weight of a push
        - the fact there is a path connecting orig and dest (if the path uses several vectors then it's a side move)
        - in case of side move, ensure all marbles can move.
     */
    def checkMove(orig: (Int, Int), dest: (Int, Int), situationBefore: Situation): Boolean = {
      // val axialMove =
      //   (convertSquarePositionToAxialCoordinate(orig), convertSquarePositionToAxialCoordinate(dest))

      val orig1D = flattenCoordinates2D(orig, situationBefore.board.width)
      val dest1D = flattenCoordinates2D(dest, situationBefore.board.width)

      // we only check moves of 1 marbles to an empty square (anywhere) for now
      if (
        situationBefore.board.pieces.contains(Pos(orig1D))
        && !situationBefore.board.pieces.contains(Pos(dest1D))
      ) {
        return true
      }

      return false
    }

    //         *             1
    //        * *            3, 4
    //       * * *           6, 10
    //      * * * *         10, 20
    //     * * * * *  <-    15, ..., 0 -> 4
    //    * * * * * *       21, ..., 5 -> 10
    //   * * * * * * *      28, ..., 11 -> 17
    //  * * * * * * * *     36, ..., 18 -> 25
    // * * * * * * * * * <- 45, ..., 26 -> 34
    //  * * * * * * * *     53, ..., 35 -> 42
    //   * * * * * * *      60, ..., 43 -> 49
    //    * * * * * *       66, ..., 50 -> 55
    //     * * * * *  <-    71, ..., 56 -> 60
    //      * * * *         75
    //       * * *          78
    //        * *           80
    //         *            81
    def flattenCoordinates2D(coordinates: (Int, Int), width: Int): Int = {
      def triangularSum(height: Int) = {
        (height * (height + 1)) / 2
      }
      val losange                    = (2 * width - 1) * (2 * width)

      if (coordinates._1 < width) {
        triangularSum(width + coordinates._1) - triangularSum(width) - coordinates._1 + coordinates._2
      } else {
        losange - triangularSum(width - 1) - triangularSum(
          (width + (2 * width - 2 - coordinates._1))
        ) - (2 * width - 1) + coordinates._2
      }
    }

// see https://www.redblobgames.com/grids/hexagons/#neighbors-cube
//             (s+)  (r-)   (q+)
//               \    |     /
//                a - - - -    s=-2
//               - - - - - -  / s=-3
//              - - - - - - -  / s=-4
//             - - - - - - - -  /
//            - - - - 0 - - - -
//           / - - - - - - - -  -- r=-1
//        s=4   - - - - - b -  --- r=-2
//               - - - - - -  ---- r=-3
//                - - - - -
//               /     /   \
//             (q-)  s=-3  (s-)
// we could see the coordinates as shortest path from the center
// a coord2D(0,0) -> {s: (width - 1) * coord2D._1, }. coord2D(4,4), coord2D(3,3), coord2D(2,2), coord2D(1,1), coord2D(0,0)

// see https://www.redblobgames.com/grids/hexagons/#neighbors-axial :
// q = r + s : we do not need to save 3 values, only 2 will be enough
// But we could probably have rs like this :
//                          r=1
//                a - - - - /   - s=4
//       s=3 -   - - - - - -   r=3
//       s=2 -  - - - - - - - /
//             - - - - - - - -  - s=1
//            - - - - 0 - - - - - s=0
//           / - - - - - - - -  - s=-1
//         -4 / - - - - - b -   - s=-2
//          -3 / - - - - - -    - s=-3
//           -2 / - - - - -     - s=-4
//            -1 / / / / /
//              0   2  r=4
// then
// axial coordinates of
// - a : {r:-4, s:4} (and q = 0)
// - b : {r:3, s:-2} (and q = -1)

    def coordinates2DtoAxialCoordinates(coordinates: (Int, Int), width: Int): (Int, Int) = ???

// when using cube or axial coordinates, see https://www.redblobgames.com/grids/hexagons/#distances
    def distance(square1: (Int, Int), square2: (Int, Int)): Int = {
      Math.max(Math.abs(square1._1 - square2._1), Math.abs(square1._2 - square2._2))
    }

  }
}

object Abalone extends App {
  val board           = AbaloneEngine.Board.fromFEN("0 0 ww1bb/wwwbbb/1ww1bb1/8/9/8/1bb1ww1/bbbwww/bb1ww b")
  println("0 0 ww1bb/wwwbbb/1ww1bb1/8/9/8/1bb1ww1/bbbwww/bb1ww b")
  println(board)
  val situationBefore = new AbaloneEngine.Situation(board, AbaloneEngine.B)
  println(situationBefore.board.pieces)
  val possibleMove1   = AbaloneEngine.Board.checkMove((2, 2), (2, 3), situationBefore)
  val possibleMove2   = AbaloneEngine.Board.checkMove((6, 1), (6, 0), situationBefore)
  val possibleMove3   = AbaloneEngine.Board.checkMove((6, 0), (6, 1), situationBefore)

  println("move from (2, 2) to (2, 3) ? " + possibleMove1)
  println("move from (6, 1) to (6, 0) ? " + possibleMove2)
  println("move from (6, 0), (6, 1) ? " + possibleMove3)
  // println(board(AbaloneEngine.Pos(1))) // it does apply
  val board7           =
    AbaloneEngine.Board.fromFEN("0 0 bbbbbbb/bbbbbbbb/3bbb3/10/11/12/5w1b5/12/11/10/3www3/wwwwwwww/wwwwwww b")
  println("0 0 bbbbbbb/bbbbbbbb/3bbb3/10/11/12/13/12/11/10/3www3/wwwwwwww/wwwwwww b")
  println(board7)
  val situationBefore7 = new AbaloneEngine.Situation(board7, AbaloneEngine.W)
  val possibleMove7    = AbaloneEngine.Board.checkMove((12, 0), (6, 5), situationBefore7)
  val possibleMove8    = AbaloneEngine.Board.checkMove((12, 0), (6, 6), situationBefore7)
  println("move from (12, 0), (6, 5) ? " + possibleMove7)
  println("move from (12, 0), (6, 6) ? " + possibleMove8)

}

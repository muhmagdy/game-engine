package Games.Chess

import Engine.{Drawable, State}

import java.awt._
import java.io.File
import javax.imageio.ImageIO
import javax.swing.JPanel

object Chess extends {
  def init(): State = {
    new ChessState()
  }

  def drawer(state: State): JPanel = {
    val alpha = Array("a", "b", "c", "d", "e", "f", "g", "h")
    val panel = new JPanel() {
      override def paint(g: Graphics): Unit = {
        super.paintComponent(g)
        g.setFont(new Font("default", Font.BOLD, 13))
        var alternateColor = true
        for (y <- 0 until 8) {
          for (x <- 0 until 8) {
            if (alternateColor) g.setColor(Color.white) //new Color(235, 235, 208)
            else g.setColor(new Color(128, 51, 0))
            g.fillRect(10 + x * 60, 15 + y * 60, 60, 60)
            g.setColor(Color.black)
            g.drawRect(10 + x * 60, 15 + y * 60, 60, 60)
            alternateColor = !alternateColor
            val p = state.drawables(y)(x)
            if (p != null) {
              g.drawImage(p.img, 25 + p.y * 60, 30 + p.x * 60, null)
            }
          }
          alternateColor = !alternateColor
          g.drawString((8 - y).toString, 0, 48 + y * 60)
          g.drawString((8 - y).toString, 493, 48 + y * 60)
          g.drawString(alpha(y), 35 + y * 60, 10)
          g.drawString(alpha(y), 35 + y * 60, 507)
        }
      }
    }
    panel setPreferredSize new Dimension(500, 510)
    panel
  }

  def controller(state: State): Boolean = {
    val chessState = state.asInstanceOf[ChessState]
    if (isValidSyntax(chessState)) {
      println(s"(syntax-checker) ${chessState.input} valid syntax :)")
      if (parseInput(chessState) && isValidMove(chessState, chessState.turn)) {
        applyMove(chessState)
        return kingIsSafe(chessState)
      }
    } else {
      println(s"(syntax-checker) ${chessState.input} invalid syntax :(")
    }
    false
  }

  def isValidSyntax(chessState: ChessState): Boolean = {
    chessState.input.matches("[a-h][1-8][a-h][1-8][qbrn]?")
  }

  /*def isValidSyntaxProm(chessState: ChessState): Boolean = {
    chessState.input.matches("q|r|b|n") && chessState.isPromoting
  }*/

  def parseInput(chessState: ChessState): Boolean = {
    val alpha = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    chessState.from = Position(56 - chessState.input.charAt(1), alpha.indexOf(chessState.input.charAt(0), 0))
    chessState.to = Position(56 - chessState.input.charAt(3), alpha.indexOf(chessState.input.charAt(2), 0))
    println("(parser-result) from => (" + chessState.from.x + ", " + chessState.from.y
      + ") to => (" + chessState.to.x + ", " + chessState.to.y + ")")
    chessState.from.x != chessState.to.x || chessState.from.y != chessState.to.y
  }

  def isValidMove(chessState: ChessState, turn: Int): Boolean = {
    val piece = chessState.drawables(chessState.from.x)(chessState.from.y).asInstanceOf[Piece]
    val dest = chessState.drawables(chessState.to.x)(chessState.to.y).asInstanceOf[Piece]
    if (piece == null || (piece.side == "black" && turn % 2 == 0) || (piece.side == "white" && turn % 2 == 1)) {
      println("(move-validator) Don't move void or pieces from other side!")
      return false
    }
    if (dest != null && ((dest.side == "black" && turn % 2 == 1) || (dest.side == "white" && turn % 2 == 0))) {
      println("(move-validator) I can't kill my teammate :(")
      return false
    }
    piece.name match {
      case "rook" => rookMove(chessState)
      case "knight" => knightMove(chessState)
      case "bishop" => bishopMove(chessState)
      case "queen" => queenMove(chessState)
      case "king" => kingMove(chessState)
      case "pawn" => pawnMove(chessState)
    }
  }

  def rookMove(chessState: ChessState): Boolean = {
    if (chessState.input.length == 5) return false
    if (chessState.from.x == chessState.to.x && chessState.from.y > chessState.to.y) { //E
      for (y <- chessState.to.y + 1 until chessState.from.y) {
        if (chessState.drawables(chessState.from.x)(y) != null) return false
      }
      return true
    } else if (chessState.from.x == chessState.to.x && chessState.from.y < chessState.to.y) { //W
      for (y <- chessState.from.y + 1 until chessState.to.y) {
        if (chessState.drawables(chessState.from.x)(y) != null) return false
      }
      return true
    } else if (chessState.from.x < chessState.to.x && chessState.from.y == chessState.to.y) { //S
      for (x <- chessState.from.x + 1 until chessState.to.x) {
        if (chessState.drawables(x)(chessState.from.y) != null) return false
      }
      return true
    } else if (chessState.from.x > chessState.to.x && chessState.from.y == chessState.to.y) { //N
      for (x <- chessState.to.x + 1 until chessState.from.x) {
        if (chessState.drawables(x)(chessState.from.y) != null) return false
      }
      return true
    }
    false
  }

  def knightMove(chessState: ChessState): Boolean = {
    chessState.input.length != 5 &&
      ((Math.abs(chessState.from.x - chessState.to.x) == 2 && Math.abs(chessState.from.y - chessState.to.y) == 1) ||
        (Math.abs(chessState.from.x - chessState.to.x) == 1 && Math.abs(chessState.from.y - chessState.to.y) == 2))
  }

  def bishopMove(chessState: ChessState): Boolean = {
    if (chessState.input.length != 5 &&
      Math.abs(chessState.from.x - chessState.to.x) == Math.abs(chessState.from.y - chessState.to.y)) {
      if (chessState.from.x > chessState.to.x && chessState.from.y < chessState.to.y) { //NE
        for (i <- 1 until (chessState.from.x - chessState.to.x)) {
          if (chessState.drawables(chessState.from.x - i)(chessState.from.y + i) != null) return false
        }
        return true
      } else if (chessState.from.x < chessState.to.x && chessState.from.y < chessState.to.y) { //SE
        for (i <- 1 until (chessState.to.x - chessState.from.x)) {
          if (chessState.drawables(chessState.from.x + i)(chessState.from.y + i) != null) return false
        }
        return true
      } else if (chessState.from.x > chessState.to.x && chessState.from.y > chessState.to.y) { //NW
        for (i <- 1 until (chessState.from.x - chessState.to.x)) {
          if (chessState.drawables(chessState.from.x - i)(chessState.from.y - i) != null) return false
        }
        return true
      } else if (chessState.from.x < chessState.to.x && chessState.from.y > chessState.to.y) { //SW
        for (i <- 1 until (chessState.to.x - chessState.from.x)) {
          if (chessState.drawables(chessState.from.x + i)(chessState.from.y - i) != null) return false
        }
        return true
      }
    }
    false
  }

  def queenMove(chessState: ChessState): Boolean = {
    chessState.input.length != 5 && (rookMove(chessState) || bishopMove(chessState))
  }

  def kingMove(chessState: ChessState): Boolean = {
    chessState.input.length != 5 && Math.abs(chessState.from.x - chessState.to.x) <= 1 &&
      Math.abs(chessState.from.y - chessState.to.y) <= 1
  }

  def pawnMove(chessState: ChessState): Boolean = {
    val dest = chessState.drawables(chessState.to.x)(chessState.to.y).asInstanceOf[Piece]
    if (chessState.turn % 2 == 0) { //white's turn
      if (chessState.from.y == chessState.to.y) { //moving straight
        if (chessState.from.x == 6 && chessState.to.x == 4) { //2 steps
          if (dest == null && chessState.drawables(5)(chessState.to.y) == null) {
            return true
          }
        } else if (chessState.from.x - chessState.to.x == 1 && dest == null) { //1 step
          if ((chessState.input.length == 5 && chessState.to.x == 0) || chessState.to.x != 0) return true
        }
      } else if (chessState.from.x - chessState.to.x == 1) { //moving diagonally
        if (Math.abs(chessState.to.y - chessState.from.y) == 1) { //to the right or left
          if (dest != null && dest.side == "black") {
            if ((chessState.input.length == 5 && chessState.to.x == 0) || chessState.to.x != 0) return true
          }
        }
      }
    } else { //black's turn
      if (chessState.from.y == chessState.to.y) { //moving straight
        if (chessState.from.x == 1 && chessState.to.x == 3) { //2 steps
          if (dest == null && chessState.drawables(2)(chessState.to.y) == null) {
            return true
          }
        } else if (chessState.to.x - chessState.from.x == 1 && dest == null) { //1 step
          if ((chessState.input.length == 5 && chessState.to.x == 7) || chessState.to.x != 7) return true
        }
      } else if (chessState.to.x - chessState.from.x == 1) { //moving diagonally
        if (Math.abs(chessState.to.y - chessState.from.y) == 1) { //to the right or left
          if (dest != null && dest.side == "white") {
            if ((chessState.input.length == 5 && chessState.to.x == 7) || chessState.to.x != 7) return true
          }
        }
      }
    }
    false
  }

  def kingIsSafe(chessState: ChessState): Boolean = {
    var color: String = ""
    val destPosition = chessState.to
    if (chessState.turn % 2 != 0) {
      color = "white"
      chessState.to = chessState.blackKingPosition
    } else {
      color = "black"
      chessState.to = chessState.whiteKingPosition
    }
    for (x <- 0 until 8) {
      for (y <- 0 until 8) {
        val piece = chessState.drawables(x)(y).asInstanceOf[Piece]
        if (piece != null && piece.side == color) {
          chessState.from = Position(x, y)
          if (isValidMove(chessState, chessState.turn + 1)) {
            restoreState(chessState, destPosition)
            return false
          }
        }
      }
    }
    true
  }

  def saveState(chessState: ChessState): Unit = {
    chessState.lastMoved = chessState.drawables(chessState.from.x)(chessState.from.y).asInstanceOf[Piece]
    chessState.lastEaten = chessState.drawables(chessState.to.x)(chessState.to.y).asInstanceOf[Piece]
  }

  def restoreState(chessState: ChessState, position: Position): Unit = {
    val lastMoved = chessState.lastMoved
    val lastEaten = chessState.lastEaten
    chessState.drawables(lastMoved.x)(lastMoved.y) = lastMoved

    if (lastMoved.name == "king") {
      if (lastMoved.side == "black") {
        chessState.blackKingPosition = Position(chessState.lastMoved.x, chessState.lastMoved.y)
      } else if (lastMoved.side == "white") {
        chessState.whiteKingPosition = Position(chessState.lastMoved.x, chessState.lastMoved.y)
      }
    }

    if (lastEaten == null) {
      chessState.drawables(position.x)(position.y) = null
    } else {
      chessState.drawables(lastEaten.x)(lastEaten.y) = lastEaten
    }
  }

  def applyMove(chessState: ChessState): Unit = {
    saveState(chessState)
    if (chessState.input.length == 5) {
      applyPromotion(chessState)
      return
    }
    val piece = chessState.drawables(chessState.from.x)(chessState.from.y).asInstanceOf[Piece]
    if (piece.name == "king" && piece.side == "white") chessState.whiteKingPosition = chessState.to
    else if (piece.name == "king" && piece.side == "black") chessState.blackKingPosition = chessState.to
    chessState.drawables(chessState.from.x)(chessState.from.y) = null
    chessState.drawables(chessState.to.x)(chessState.to.y) =
      Piece(piece.name, piece.side, chessState.to.x, chessState.to.y)
  }

  def applyPromotion(chessState: ChessState): Unit = {
    chessState.drawables(chessState.from.x)(chessState.from.y) = null
    var color: String = ""
    if (chessState.turn % 2 == 0) {
      color = "white"
    } else {
      color = "black"
    }
    chessState.input.charAt(4) match {
      case 'q' => chessState.drawables(chessState.to.x)(chessState.to.y) =
        Piece("queen", color, chessState.to.x, chessState.to.y)
      case 'r' => chessState.drawables(chessState.to.x)(chessState.to.y) =
        Piece("rook", color, chessState.to.x, chessState.to.y)
      case 'b' => chessState.drawables(chessState.to.x)(chessState.to.y) =
        Piece("bishop", color, chessState.to.x, chessState.to.y)
      case 'n' => chessState.drawables(chessState.to.x)(chessState.to.y) =
        Piece("knight", color, chessState.to.x, chessState.to.y)
    }
  }

  case class Piece(name: String, side: String, i: Int, j: Int) extends Drawable {
    override var img: Image = ImageIO.read(new File(s"src/Games/Chess/assets/$side/$name.png"))
      .getScaledInstance(30, 30, Image.SCALE_SMOOTH)
    override var x: Int = i
    override var y: Int = j
  }

  case class Position(x: Int, y: Int)

  class ChessState extends State {
    this.drawables = Array.ofDim[Drawable](8, 8)
    this.drawables(0)(0) = Piece("rook", "black", 0, 0)
    this.drawables(0)(1) = Piece("knight", "black", 0, 1)
    this.drawables(0)(2) = Piece("bishop", "black", 0, 2)
    this.drawables(0)(3) = Piece("queen", "black", 0, 3)
    this.drawables(0)(4) = Piece("king", "black", 0, 4)
    this.drawables(0)(5) = Piece("bishop", "black", 0, 5)
    this.drawables(0)(6) = Piece("knight", "black", 0, 6)
    this.drawables(0)(7) = Piece("rook", "black", 0, 7)
    for (i <- 0 until 8) {
      this.drawables(1)(i) = Piece("pawn", "black", 1, i)
      this.drawables(6)(i) = Piece("pawn", "white", 6, i)
    }
    this.drawables(7)(0) = Piece("rook", "white", 7, 0)
    this.drawables(7)(1) = Piece("knight", "white", 7, 1)
    this.drawables(7)(2) = Piece("bishop", "white", 7, 2)
    this.drawables(7)(3) = Piece("queen", "white", 7, 3)
    this.drawables(7)(4) = Piece("king", "white", 7, 4)
    this.drawables(7)(5) = Piece("bishop", "white", 7, 5)
    this.drawables(7)(6) = Piece("knight", "white", 7, 6)
    this.drawables(7)(7) = Piece("rook", "white", 7, 7)
    //var isPromoting: Boolean = false
    var from: Position = Position(0, 0)
    var to: Position = Position(0, 0)
    var whiteKingPosition: Position = Position(7, 4)
    var blackKingPosition: Position = Position(0, 4)
    var lastMoved: Piece = null
    var lastEaten: Piece = null
  }
}

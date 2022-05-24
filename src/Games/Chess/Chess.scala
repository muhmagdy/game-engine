package Games.Chess

import Engine.{Drawable, State}

import java.awt.{Color, Dimension, Graphics, Image}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.JPanel

object Chess extends {
  case class Piece(name: String, side: String, i: Int, j: Int) extends Drawable {
    override var img: Image = ImageIO.read(new File(s"src/Games/Chess/assets/$side/$name.png"))
                                      .getScaledInstance(30, 30, Image.SCALE_SMOOTH)
    override var x: Int = i
    override var y: Int = j
  }

  case class Position(x: Int, y: Int)

  class ChessState extends State {
    this.drawables = Array.ofDim[Drawable](8,8)
    this.drawables(0)(0) = Piece("rook", "black", 0, 0)
    this.drawables(0)(1) = Piece("knight", "black", 0, 1)
    this.drawables(0)(2) = Piece("bishop", "black", 0, 2)
    this.drawables(0)(3) = Piece("queen", "black", 0, 3)
    this.drawables(0)(4) = Piece("king", "black", 0, 4)
    this.drawables(0)(5) = Piece("bishop", "black", 0, 5)
    this.drawables(0)(6) = Piece("knight", "black", 0, 6)
    this.drawables(0)(7) = Piece("rook", "black", 0, 7)
    for(i <- 0 until(8) ) {
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
    var isPromoting = false
    var from: Position = Position(0, 0)
    var to: Position = Position(0, 0)
  }

  def init(): State = {
    new ChessState()
  }

  def drawer(state: State): JPanel = {
    val alpha = Array("a", "b", "c", "d", "e", "f", "g", "h")
    val panel = new JPanel() {
      override def paint(g: Graphics): Unit = {
        super.paintComponent(g)
        var alternateColor = true
        for (y <- 0 until 8) {
          for (x <- 0 until 8) {
            if (alternateColor) g.setColor(new Color(235, 235, 208))
            else g.setColor(new Color(120, 69, 69))
            g.fillRect(10 + x * 60, 10 + y * 60, 60, 60)
            g.setColor(Color.black)
            g.drawRect(10+ x * 60, 10 + y * 60, 60, 60)
            alternateColor = !alternateColor
            val p = state.drawables(y)(x)
            if(p != null) {
              g.drawImage(p.img, 25 + p.y * 60, 25 + p.x * 60, null)
            }
          }
          alternateColor = !alternateColor
          g.drawString((8-y).toString, 0, 40 + y * 60)
          g.drawString((8-y).toString, 493, 40 + y * 60)
          g.drawString(alpha(y), 35 + y * 60, 10)
          g.drawString(alpha(y), 35 + y * 60, 500)
        }
      }
    }
    panel setPreferredSize new Dimension(500, 510)
    //panel setBorder BorderFactory.createLineBorder(Color.black);
    panel
  }

  def controller(state: State): Boolean = {
    if(isValidSyntaxNorm(state.asInstanceOf[ChessState])) {
      println(s"(syntax-checker) ${state.input} valid syntax :)")
      state.turn += 1
      parse(state.asInstanceOf[ChessState])
      if(true) { //checking the validity of move
        applyMove(state.asInstanceOf[ChessState])
        return true
      }
      return false
    } else if(isValidSyntaxProm(state.asInstanceOf[ChessState])) {
      println(s"(syntax-checker) ${state.input} valid syntax :)")
      state.turn += 1
      state.asInstanceOf[ChessState].isPromoting = false
      return true
    }
    println(s"(syntax-checker) ${state.input} invalid syntax :(")
    false
  }

  def isValidSyntaxNorm(chessState: ChessState): Boolean = {
    chessState.input.matches( "[a-h][1-8][a-h][1-8]") && !chessState.isPromoting
  }

  def isValidSyntaxProm(chessState: ChessState): Boolean = {
    chessState.input.matches("r|b|n|q") && chessState.isPromoting
  }

  def parse(chessState: ChessState): Unit = {
    val alpha = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    chessState.from = Position(56 - chessState.input.charAt(1), alpha.indexOf(chessState.input.charAt(0), 0))
    chessState.to = Position(56 - chessState.input.charAt(3), alpha.indexOf(chessState.input.charAt(2), 0))
    println("(parser-result) from => " + chessState.from.x + " " + chessState.from.y)
    println("(parser-result) to => " + chessState.to.x + " " + chessState.to.y)
  }

  def applyMove(chessState: ChessState): Unit = {
    val piece = chessState.drawables(chessState.from.x)(chessState.from.y).asInstanceOf[Piece]
    chessState.drawables(chessState.from.x)(chessState.from.y) = null
    chessState.drawables(chessState.to.x)(chessState.to.y) =
      Piece(piece.name, piece.side ,chessState.to.x, chessState.to.y)
  }


}

package Games.Chess

import Engine.{Drawable, State}

import java.awt.{Color, Dimension, Graphics, Image}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.{JPanel}

object Chess extends {
  case class Piece(name: String, side: String, i: Int, j: Int) extends Drawable {
    override var img: Image = ImageIO.read(new File(s"src/Games/Chess/assets/$side/$name.png"))
                                      .getScaledInstance(60, 60, Image.SCALE_SMOOTH)
    override var x: Int = i
    override var y: Int = j
  }

  class ChessState extends State {
    this.drawables = Array.ofDim[Drawable](8,8)
    this.drawables(0)(0) = Piece("rook", "white", 0, 0)
    this.drawables(0)(1) = Piece("knight", "white", 0, 1)
    this.drawables(0)(2) = Piece("bishop", "white", 0, 2)
    this.drawables(0)(3) = Piece("queen", "white", 0, 3)
    this.drawables(0)(4) = Piece("king", "white", 0, 4)
    this.drawables(0)(5) = Piece("bishop", "white", 0, 5)
    this.drawables(0)(6) = Piece("knight", "white", 0, 6)
    this.drawables(0)(7) = Piece("rook", "white", 0, 7)
    for(i <- 0 until(8) ) {
      this.drawables(1)(i) = Piece("pawn", "white", 1, i)
      this.drawables(6)(i) = Piece("pawn", "black", 6, i)
    }
    this.drawables(7)(0) = Piece("rook", "black", 7, 0)
    this.drawables(7)(1) = Piece("knight", "black", 7, 1)
    this.drawables(7)(2) = Piece("bishop", "black", 7, 2)
    this.drawables(7)(3) = Piece("queen", "black", 7, 3)
    this.drawables(7)(4) = Piece("king", "black", 7, 4)
    this.drawables(7)(5) = Piece("bishop", "black", 7, 5)
    this.drawables(7)(6) = Piece("knight", "black", 7, 6)
    this.drawables(7)(7) = Piece("rook", "black", 7, 7)
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
            else g.setColor(new Color(119, 148, 85))
            g.fillRect(10 + x * 60, 10 + y * 60, 60, 60)
            alternateColor = !alternateColor
            g.setColor(Color.black)
            if(y == 0 || y == 7) {
              g.drawString((x+1).toString, 35 + x * 60, 10)
              g.drawString((x+1).toString, 35 + x * 60, 500)
            }
            val p = state.drawables(y)(x)
            if(p != null) {
              g.drawImage(p.img, 10 + p.y * 60, 10 + p.x * 60, null)
            }
          }
          alternateColor = !alternateColor
          g.drawString(alpha(y), 0, 40 + y * 60)
          g.drawString(alpha(y), 492, 40 + y * 60)
        }
      }
    }
    panel setPreferredSize new Dimension(500, 500)
    //panel setBorder BorderFactory.createLineBorder(Color.black);
    panel
  }

  def controller(state: State): Boolean = ???
}
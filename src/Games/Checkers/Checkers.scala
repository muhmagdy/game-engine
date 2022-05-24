package Games.Checkers

import Engine.{Drawable, State}

import java.awt.{Color, Dimension, Graphics, Image}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.JPanel

object Checkers {

  /* There are 2 modes for a piece "regular" which only can moves forward, and "super" which can moves forward and backward */
  case class Piece(mode: String, side: String, i: Int, j: Int) extends Drawable {
    override var img: Image = ImageIO.read(new File(s"src/Games/Checkers/assets/$side-$mode.png"))
      .getScaledInstance(60, 60, Image.SCALE_SMOOTH)
    override var x: Int = i
    override var y: Int = j
  }

  class CheckersState extends State {
    this.drawables = Array.ofDim[Drawable](8,8)
    for(i <- 0 until 3){
      for(j <- 0 until 8) {
        if (i % 2 == 0 && j % 2 == 1)
          drawables(i)(j) = Piece("regular", "black", i, j)
        else if(i % 2 == 1 && j % 2 == 0)
          drawables(i)(j) = Piece("regular", "black", i, j)
      }
    }
    for(i <- 5 until 8){
      for(j <- 0 until 8) {
        if (i % 2 == 0 && j % 2 == 1)
          drawables(i)(j) = Piece("regular", "white", i, j)
        else if(i % 2 == 1 && j % 2 == 0)
          drawables(i)(j) = Piece("regular", "white", i, j)
      }
    }
  }

  def init(): State = {
    new CheckersState
  }

  def drawer(state: State): JPanel = {
    val alpha = Array("a", "b", "c", "d", "e", "f", "g", "h")
    val panel = new JPanel() {
      override def paint(g: Graphics): Unit = {
        import java.awt.Font
        g.setFont(new Font("default", Font.BOLD, 13))
        super.paintComponent(g)
        var alternateColor = true
        for (y <- 0 until 8) {
          for (x <- 0 until 8) {
            if (alternateColor) g.setColor(new Color(229, 228, 226))
            else g.setColor(new Color(113, 121, 126))
            g.fillRect(10 + x * 60, 10 + y * 60+5, 60, 60)
            alternateColor = !alternateColor
            g.setColor(Color.black)
            if(y == 0 || y == 7) {
              g.drawString((x+1).toString, 35 + x * 60, 10)
              g.drawString((x+1).toString, 35 + x * 60, 510)
            }
            val p = state.drawables(y)(x)
            if(p != null) {
              g.drawImage(p.img, 10 + p.y * 60, 15 + p.x * 60, null)
            }
          }
          alternateColor = !alternateColor
          g.drawString(alpha(y), 0, 50 + y * 60)
          g.drawString(alpha(y), 493, 50 + y * 60)
        }
      }
    }
    panel setPreferredSize new Dimension(500, 510)
    //panel setBorder BorderFactory.createLineBorder(Color.black);
    panel
  }

  def controller(state: State): Boolean = ???
}



package Games.TicTacToe

import java.awt.{Dimension, Graphics, Image}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.JPanel

import Engine._

object TicTacToe {
  case class X(i: Int, j: Int) extends Drawable {
    override var img: Image = ImageIO.read(new File("src/Games/TicTacToe/assets/o.png"))
                                      .getScaledInstance(160, 160, Image.SCALE_SMOOTH)
    override var x: Int = i
    override var y: Int = j
  }

  case class O(i: Int, j: Int) extends Drawable {
    override var img: Image = ImageIO.read(new File("src/Games/TicTacToe/assets/x.png"))
                                      .getScaledInstance(160, 160, Image.SCALE_SMOOTH)
    override var x: Int = i
    override var y: Int = j
  }

  def drawer(state: State): JPanel = {
    val alpha = Array("a", "b", "c")
    state.drawables = Array(Array(O(1, 0), X(0, 1)))
    val panel = new JPanel() {
      override def paint(g: Graphics): Unit = {
        super.paintComponent(g)
        import java.awt.Color
        var white = true
        for (y <- 0 until 3) {
          for (x <- 0 until 3) {
            if (white) g.setColor(new Color(235, 235, 208))
            else g.setColor(new Color(119, 148, 85))
            g.fillRect(10 + x * 160, 10 + y * 160, 160, 160)
            white = !white
            g.setColor(Color.black)
            g.drawString((x + 1).toString, 90 + x * 160, 10)
            g.drawString((x + 1).toString, 90 + x * 160, 500)
          }
          g.drawString(alpha(y), 0, 90 + y * 160)
          g.drawString(alpha(y), 490, 90 + y * 160)
        }
        for (p <- state.drawables(0)) {
          println("Drawing")
          g.drawImage(p.img, 10 + p.x * 160, 10 + p.y * 160, null)
        }
      }
    }
    panel setPreferredSize new Dimension(500, 500)
    //panel setBorder BorderFactory.createLineBorder(Color.black);
    panel
  }

  def controller(state: State): Nothing = ???
}

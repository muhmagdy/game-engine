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

  class TicState extends State {
    this.drawables = Array.ofDim[Drawable](3,3)
  }

  def init(): State = {
    new TicState
  }

  def drawer(state: State): JPanel = {
    val alpha = Array("a", "b", "c")
    val panel = new JPanel() {
      override def paint(g: Graphics): Unit = {
        super.paintComponent(g)
        import java.awt.Color
        var white = true
        for (y <- 0 until 3) {
          for (x <- 0 until 3) {
            if (white) g.setColor(new Color(229, 228, 226))
            else g.setColor(new Color(113, 121, 126))
            g.fillRect(10 + x * 160, 10 + y * 160, 160, 160)
            white = !white
            g.setColor(Color.black)
            g.drawString((x + 1).toString, 90 + x * 160, 10)
            g.drawString((x + 1).toString, 90 + x * 160, 500)
          }
          g.drawString(alpha(y), 0, 90 + y * 160)
          g.drawString(alpha(y), 490, 90 + y * 160)
        }
        for(row <- state.drawables){
          for (p <- row) {
            if(p != null) {
              println("Drawing")
              g.drawImage(p.img, 10 + p.x * 160, 10 + p.y * 160, null)
            }
          }
        }
      }
    }
    panel setPreferredSize new Dimension(500, 500)
    //panel setBorder BorderFactory.createLineBorder(Color.black);
    panel
  }

  def controller(state: State): Boolean = {
    val alpha = Array('a', 'b', 'c')
    if(!(state.input.length == 2) || !state.input(0).isDigit || !alpha.contains(state.input(1)))
      return false
    val i = state.input.substring(0,1).toInt - 1
    val j = alpha.indexOf(state.input(1), 0)
    if(i >= 3 || j >= 3 )
      return false
    if(state.drawables(i)(j) != null)
      return false
    if(state.turn%2 == 0) {
      state.drawables(i)(j) = X(i, j)
    } else{
      state.drawables(i)(j) = O(i, j)
    }
    state.turn += 1
    true
  }

  private def parse(input: String): (Int, Int) = {
    val alpha = Array("a", "b", "c")
    val i = alpha.indexOf(input.substring(0,0), 0)
    val j = input.substring(1,1).toInt
    (i,j)
  }
}

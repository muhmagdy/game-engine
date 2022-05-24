package Games.TicTacToe

import java.awt.{Dimension, Graphics, Image, Font, Color}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.JPanel
import Engine._

object TicTacToe {

  val xImg: Image = ImageIO.read(new File("src/Games/TicTacToe/assets/x.png"))
    .getScaledInstance(160, 160, Image.SCALE_SMOOTH)

  val oImg: Image = ImageIO.read(new File("src/Games/TicTacToe/assets/o.png"))
    .getScaledInstance(160, 160, Image.SCALE_SMOOTH)

  case class xoSymbol(i: Int, j: Int, xoImage: Image) extends Drawable{
    override var x: Int = i
    override var y: Int = j
    override var img: Image = xoImage
  }

  class TicState extends State {
    this.drawables = Array.ofDim[Drawable](3,3)
  }

  def init(): State = {
    new TicState
  }

  private def drawBoard(g: Graphics): Unit = {
    g.setFont(new Font("default", Font.BOLD, 13))
    val alpha = Array("a", "b", "c")
    var white = true
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        if (white) g.setColor(new Color(229, 228, 226))
        else g.setColor(new Color(113, 121, 126))
        g.fillRect(10 + x * 160, 15 + y * 160, 160, 160)
        white = !white
        g.setColor(Color.black)
        g.drawString((x + 1).toString, 90 + x * 160, 10)
        g.drawString((x + 1).toString, 90 + x * 160, 510)
      }
      g.drawString(alpha(y), 0, 95 + y * 160)
      g.drawString(alpha(y), 490, 95 + y * 160)
    }
  }

  private def drawSymbol(d: Drawable, g: Graphics): Unit =
    if ( d!=null ) g.drawImage(d.img, 10 + d.x * 160, 15 + d.y * 160, null)

  def drawer(state: State): JPanel = {
    val panel = new JPanel() {
      override def paint(g: Graphics): Unit = {
        super.paintComponent(g)
        drawBoard(g)
        state.drawables.foreach(_.foreach(drawSymbol(_,g)))
      }
    }
    panel setPreferredSize new Dimension(500, 510)
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
      state.drawables(i)(j) = xoSymbol(i, j, xImg)
    } else{
      state.drawables(i)(j) = xoSymbol(i, j, oImg)
    }
    //state.turn += 1
    true
  }
}

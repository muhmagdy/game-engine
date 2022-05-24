package Games.Connect_4

import java.awt.{Color, Dimension, Graphics, Image}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.{BorderFactory, JPanel}
import Engine.{Drawable, State}
import java.awt.image.BufferedImage
import javax.swing.JLabel
import javax.swing.ImageIcon

object Connect4 {
    val path = "src/Games/Connect_4"
    case class chip(i: Int, j: Int, color: String) extends Drawable{
        override var img: Image = ImageIO.read(new File(s"$path/assets/$color.png")).getScaledInstance(60, 70, Image.SCALE_SMOOTH)
        override var x: Int = j
        override var y: Int = i
    }
    class Connect4State extends State {
        var top:Array[Int] = new Array[Int](7)
        this.drawables = Array.ofDim[Drawable](6,7)
    }


  def init(): State = {
    new Connect4State()
  }
  def draw(x: Drawable, g: Graphics) = {
      if (x != null){
        println("Drawing")
        g.drawImage(x.img, 7 + x.x * 71, 11 + x.y * 78, null)
      }

  }
  def drawArray(arr: Array[Drawable], g: Graphics) = {
    arr.foreach(draw(_, g))
  }
  def drawer(state: State): JPanel = {
    val panel = new JPanel() {
      override def paint(g: Graphics): Unit = {
        super.paintComponent(g)
        var boardImg: Image = ImageIO.read(new File(s"$path/assets/board.png"))
        g.drawImage(boardImg, 0, 0, this)
        println("drawing")
        state.drawables.foreach(drawArray(_, g))
      }
    }
    panel setPreferredSize new Dimension(500,500)
    panel
  }

  def getInput(state: State): Int = {
    var x: Int = 0
    x
  }

  def controller(state: State): Boolean = {
      var myState: Connect4State = state.asInstanceOf[Connect4State]
      var rv = false
      try{
        var x = myState.input.toInt - 1
        if (x < 7 && myState.top(x) < 6){
          var y = 5 - myState.top(x)
          var color = "red"
          if(state.turn % 2 == 0)    color = "yellow"  
          myState.drawables(y)(x) = chip(y, x, color)
          myState.top(x) = myState.top(x) + 1
          rv = true
        } 
      }catch {
        case _: Throwable => {}
      }
      rv
  }
}

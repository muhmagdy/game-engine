package Games.Chess

import Engine._

import java.awt.Image
import java.io.File
import javax.imageio.ImageIO

object Chess {
  case class Piece(name: String, side: String, i: Int, j: Int) extends Drawable {
    override var img: Image = ImageIO.read(new File("src/Games/Chess/assets/$side/$name.png"))
                                      .getScaledInstance(25, 25, Image.SCALE_SMOOTH)
    override var x: Int = i
    override var y: Int = j
  }
}

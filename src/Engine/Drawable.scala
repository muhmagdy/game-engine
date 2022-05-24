package Engine

import java.awt.Image

trait Drawable {
  var x: Int
  var y: Int
  var img: Image

  override def toString: String = {
    "(" + x.toString + "," + y.toString + ")"
  }
}

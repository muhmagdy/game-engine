package Engine

class State {
  var drawables: Array[Array[Drawable]] = Array()
  var turn: Int = 0
  var input: String = ""

  override def toString: String = {
    var s = "{"
    for(row <- drawables){
      s += "{"
      for(d <- row){
        if(d == null)
          s += "null,"
        else s += d.toString + ","
      }
      s += "},"
    }
    s += "}"
    s
  }
}

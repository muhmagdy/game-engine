package Games.Checkers

import Engine.{Drawable, State}

import java.awt.{Color, Dimension, Graphics, Image}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.JPanel
import scala.util.control.Breaks.{break, breakable}

object Checkers {

  /* There are 2 modes for a piece "regular" which only can moves forward, and "super" which can moves forward and backward */
  case class Piece(mode: String, side: String, i: Int, j: Int) extends Drawable {
    override var img: Image = ImageIO.read(new File(s"resources/checkers/$side-$mode.png"))
      .getScaledInstance(60, 60, Image.SCALE_SMOOTH)
    override var x: Int = i
    override var y: Int = j
  }

  case class Position(x: Int, y: Int)

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
    var from: Position = Position(0, 0)
    var to: Position = Position(0, 0)
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
            val p = state.drawables(y)(x)
            if(p != null) {
              g.drawImage(p.img, 10 + p.y * 60, 15 + p.x * 60, null)
            }
          }
          alternateColor = !alternateColor
          g.drawString((8 - y).toString, 0, 48 + y * 60)
          g.drawString((8 - y).toString, 493, 48 + y * 60)
          g.drawString(alpha(y), 35 + y * 60, 10)
          g.drawString(alpha(y), 35 + y * 60, 507)
        }
      }
    }
    panel setPreferredSize new Dimension(500, 510)
    //panel setBorder BorderFactory.createLineBorder(Color.black);
    panel
  }

  def controller(state: State): Boolean = {
    val checkersState = state.asInstanceOf[CheckersState]
    if(!isValidSyntaxNorm(checkersState)){
      println(s"(syntax-checker) ${state.input} invalid syntax :(")
      return false
    }
    if(!parseInput(checkersState) || !isValidMove(checkersState)){
      return false
    }

    applyMove(checkersState)
  }

  def parseInput(checkersState: CheckersState): Boolean = {
    val alpha = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    checkersState.from = Position( 56 - checkersState.input.charAt(1) , alpha.indexOf(checkersState.input.charAt(0), 0))
    checkersState.to = Position( 56 - checkersState.input.charAt(3), alpha.indexOf(checkersState.input.charAt(2), 0))
    println("(parser-result) from => (" + checkersState.from.x + ", " + checkersState.from.y
      + ") to => (" + checkersState.to.x + ", " + checkersState.to.y + ")")
    checkersState.from.x != checkersState.to.x || checkersState.from.y != checkersState.to.y
  }

  def isValidSyntaxNorm(checkersState: CheckersState): Boolean = {
    checkersState.input.matches("[a-h][1-8][a-h][1-8]")
  }

  def isValidMove(checkersState: CheckersState): Boolean = {
    val from = checkersState.from
    val to = checkersState.to
    val piece = checkersState.drawables(from.x)(from.y).asInstanceOf[Piece]
    val turn = checkersState.turn
    if (piece == null || (piece.side == "black" && turn % 2 == 0) || (piece.side == "white" && turn % 2 == 1)){
      // check that the moving piece is valid
      println("(move-validator) Don't move void or pieces from other side!")
      return false
    }
    if(checkersState.drawables(to.x)(to.y).asInstanceOf[Piece]!= null){
      //check that the to is valid
      println("(move-validator) Don't move piece to other piece!")
      return false
    }
    var steps = 0
    if(Math.abs(from.x-to.x)==Math.abs(from.y-to.y) ){
      steps = Math.abs(from.x-to.x)
      if(steps!= 1 && steps!=2){
        println("(move-validator) Don't move to many steps!")
        return false
      }
    }else{
      println("(move-validator) Invalid move!")
      return false
    }
    if(piece.mode.equals("regular")){
      if((piece.side.equals("white") && to.x> from.x) || (piece.side.equals("black") && to.x< from.x)){
        println("(move-validator) Regular pieces move forward only!")
        return false
      }
    }

    val capPoses = allCap(checkersState)
    if(capPoses.nonEmpty){
      if(steps!=2){
        println("(move-validator) you MUST capture")
        return false
      }
      val destpos : Position = Position((from.x+to.x)/2,(from.y+to.y)/2)
      if(!capPoses.exists({pos=>pos.x==destpos.x && pos.y==destpos.y })){
        println("(move-validator) You can not capture this!")
        return false
      }
      checkersState.drawables(destpos.x)(destpos.y) = null
      checkersState.turn -= 1
    }

    true
  }

  def allCap(checkersState: CheckersState): List[Position] ={
    val side:String = if(checkersState.turn%2==0){"white"} else {"black"}
    var res: List[Position] = List()
    for(i <- 0 until 8){
      for(j <- 0 until 8) {
          var piece: Piece =  checkersState.drawables(i)(j).asInstanceOf[Piece]
          if(piece != null && piece.side.equals(side))
            res = res ::: captures(checkersState, piece)

      }
    }
    res
  }

  def captures(checkersState: CheckersState, piece:Piece): List[Position] = {
    var res: List[Position] = List[Position]()

    var poses:List[Position] = List()
    var enemy: String = ""
    if(piece.mode.equals("super")){
      poses = List(Position(1,1), Position(1,-1), Position(-1,1), Position(-1,-1))
      if(piece.side.equals("black")){
        enemy = "white"
      }else{
        enemy = "black"
      }
    }else if(piece.side.equals("black")){
      poses = List(Position(1,1), Position(1,-1))
      enemy = "white"
    }else{
      poses = List(Position(-1,1), Position(-1,-1))
      enemy = "black"
    }
    val x = piece.x
    val y = piece.y
    for(pos<-poses){
      breakable{
        if(2*pos.x+x< 0 || 2*pos.x+x> 7 || 2*pos.y+y< 0 || 2*pos.y+y> 7) {
          break
        }
        val cap = checkersState.drawables(x + pos.x)(y + pos.y).asInstanceOf[Piece]

        if(cap!= null && cap.side.equals(enemy) && checkersState.drawables(x + 2*pos.x)(y + 2*pos.y).asInstanceOf[Piece]==null){
           res = res :+ Position(x + pos.x , y + pos.y )
        }
      }
    }
    res
  }


  def applyMove(checkersState: CheckersState): Boolean = {
    val piece = checkersState.drawables(checkersState.from.x)(checkersState.from.y).asInstanceOf[Piece]
    checkersState.drawables(checkersState.from.x)(checkersState.from.y) = null
    if(piece.mode.equals("regular") &&  (checkersState.to.x==0 ||checkersState.to.x==7)){
      checkersState.drawables(checkersState.to.x)(checkersState.to.y) = Piece("super", piece.side, checkersState.to.x, checkersState.to.y)
    }else{
      checkersState.drawables(checkersState.to.x)(checkersState.to.y) = Piece(piece.mode, piece.side, checkersState.to.x, checkersState.to.y)
    }
    true
  }



}



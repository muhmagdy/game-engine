package Games.Chess

import Engine.{Drawable, State}

import java.awt.{Color, Dimension, Graphics, Image}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.JPanel

object Chess extends {
  case class Piece(name: String, side: String, i: Int, j: Int) extends Drawable {
    override var img: Image = ImageIO.read(new File(s"src/Games/Chess/assets/$side/$name.png"))
                                      .getScaledInstance(30, 30, Image.SCALE_SMOOTH)
    override var x: Int = i
    override var y: Int = j
  }

  case class Position(x: Int, y: Int)

  class ChessState extends State {
    this.drawables = Array.ofDim[Drawable](8,8)
    this.drawables(0)(0) = Piece("rook", "black", 0, 0)
    this.drawables(0)(1) = Piece("knight", "black", 0, 1)
    this.drawables(0)(2) = Piece("bishop", "black", 0, 2)
    this.drawables(0)(3) = Piece("queen", "black", 0, 3)
    this.drawables(0)(4) = Piece("king", "black", 0, 4)
    this.drawables(0)(5) = Piece("bishop", "black", 0, 5)
    this.drawables(0)(6) = Piece("knight", "black", 0, 6)
    this.drawables(0)(7) = Piece("rook", "black", 0, 7)
    for(i <- 0 until 8 ) {
      this.drawables(1)(i) = Piece("pawn", "black", 1, i)
      this.drawables(6)(i) = Piece("pawn", "white", 6, i)
    }
    this.drawables(7)(0) = Piece("rook", "white", 7, 0)
    this.drawables(7)(1) = Piece("knight", "white", 7, 1)
    this.drawables(7)(2) = Piece("bishop", "white", 7, 2)
    this.drawables(7)(3) = Piece("queen", "white", 7, 3)
    this.drawables(7)(4) = Piece("king", "white", 7, 4)
    this.drawables(7)(5) = Piece("bishop", "white", 7, 5)
    this.drawables(7)(6) = Piece("knight", "white", 7, 6)
    this.drawables(7)(7) = Piece("rook", "white", 7, 7)
    var isPromoting: Boolean = false
    var from: Position = Position(0, 0)
    var to: Position = Position(0, 0)
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
            else g.setColor(new Color(120, 69, 69))
            g.fillRect(10 + x * 60, 10 + y * 60, 60, 60)
            g.setColor(Color.black)
            g.drawRect(10+ x * 60, 10 + y * 60, 60, 60)
            alternateColor = !alternateColor
            val p = state.drawables(y)(x)
            if(p != null) {
              g.drawImage(p.img, 25 + p.y * 60, 25 + p.x * 60, null)
            }
          }
          alternateColor = !alternateColor
          g.drawString((8-y).toString, 0, 40 + y * 60)
          g.drawString((8-y).toString, 493, 40 + y * 60)
          g.drawString(alpha(y), 35 + y * 60, 10)
          g.drawString(alpha(y), 35 + y * 60, 500)
        }
      }
    }
    panel setPreferredSize new Dimension(500, 510)
    //panel setBorder BorderFactory.createLineBorder(Color.black);
    panel
  }

  def controller(state: State): Boolean = {
    val chessState = state.asInstanceOf[ChessState]
    if(isValidSyntaxNorm(chessState)) {
      println(s"(syntax-checker) ${chessState.input} valid syntax :)")
      parseInput(chessState)
      if(doesExist(state.asInstanceOf[ChessState]) && isValidMove(state.asInstanceOf[ChessState])) { //checking the validity of move
        return applyMove(chessState)
      }
      return false
    } else if(isValidSyntaxProm(chessState)) {
      println(s"(syntax-checker) ${chessState.input} valid syntax :)")
      return applyPromotion(chessState)
    }
    println(s"(syntax-checker) ${state.input} invalid syntax :(")
    false
  }

  def isValidSyntaxNorm(chessState: ChessState): Boolean = {
    chessState.input.matches( "[a-h][1-8][a-h][1-8]") && !chessState.isPromoting
  }

  def isValidSyntaxProm(chessState: ChessState): Boolean = {
    chessState.input.matches("q|r|b|n") && chessState.isPromoting
  }

  def parseInput(chessState: ChessState): Unit = {
    val alpha = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    chessState.from = Position(56 - chessState.input.charAt(1), alpha.indexOf(chessState.input.charAt(0), 0))
    chessState.to = Position(56 - chessState.input.charAt(3), alpha.indexOf(chessState.input.charAt(2), 0))
    println("(parser-result) from => " + chessState.from.x + " " + chessState.from.y)
    println("(parser-result) to => " + chessState.to.x + " " + chessState.to.y)
  }

  def applyMove(chessState: ChessState): Boolean = {
    val piece = chessState.drawables(chessState.from.x)(chessState.from.y).asInstanceOf[Piece]
    chessState.drawables(chessState.from.x)(chessState.from.y) = null
    chessState.drawables(chessState.to.x)(chessState.to.y) =
      Piece(piece.name, piece.side ,chessState.to.x, chessState.to.y)
    if(!chessState.isPromoting) chessState.turn += 1
    true
  }

  def doesExist(chessState: ChessState): Boolean = {
    val supposedTurn: Int = chessState.turn % 2
    var supposedColor: String = ""
    supposedTurn match {
      case 0 => supposedColor = "white"
      case 1 => supposedColor = "black"
    }
    var actualColor: String = ""
    if(chessState.drawables(chessState.from.x)(chessState.from.y)!=null){
      actualColor = chessState.drawables(chessState.from.x)(chessState.from.y).asInstanceOf[Piece].side
    }
    println("supposedColor = " + supposedColor + " && actualColor = " + actualColor)
    if(supposedColor != actualColor)
      return false
    true
  }

  def isValidMove(c: ChessState): Boolean = {
    val supposedPiece: String = c.drawables(c.from.x)(c.from.y).asInstanceOf[Piece].name
    supposedPiece match {
      case "rook" => rookMove(c)
      case "knight" => knightMove(c)
      case "bishop" => bishopMove(c)
      case "queen" => queenMove(c)
      case "king" => kingMove(c)
      case "pawn" => pawnMove(c)
    }
  }

  def rookMove(chessState: ChessState): Boolean = {
    (  (chessState.from.x - chessState.to.x != 0 && chessState.from.y - chessState.to.y ==0)
    || (chessState.from.x - chessState.to.x == 0 && chessState.from.y - chessState.to.y !=0)
    )
  }
  def knightMove(chessState: ChessState): Boolean = {
    (  (Math.abs(chessState.from.x - chessState.to.x) == 2 && Math.abs(chessState.from.y - chessState.to.y) ==1)
    || (Math.abs(chessState.from.x - chessState.to.x) == 1 && Math.abs(chessState.from.y - chessState.to.y) ==2)
    )
  }
  def bishopMove(chessState: ChessState): Boolean = {
    Math.abs(chessState.from.x - chessState.to.x) == Math.abs(chessState.from.y - chessState.to.y)
  }
  def queenMove(chessState: ChessState): Boolean = {
    rookMove(chessState) || bishopMove(chessState)
  }
  def kingMove(chessState: ChessState): Boolean = {
    ( (Math.abs(chessState.from.x - chessState.to.x) <= 1 && Math.abs(chessState.from.y - chessState.to.y) <=1)
    &&(Math.abs(chessState.from.x - chessState.to.x) + Math.abs(chessState.from.y - chessState.to.y))>0
    )
  }
  def pawnMove(chessState: ChessState): Boolean = {
    if(!verifyMoveToFront(chessState))
      return false
    val position : String = pawnPosition(chessState)
    position match {
      case "initial" => chessState.from.y-chessState.to.y==0 && Math.abs(chessState.from.x-chessState.to.x)<=2
      case "willPromote" =>
        if(!(chessState.from.y-chessState.to.y==0 && Math.abs(chessState.from.x-chessState.to.x)==1))
          return false
        chessState.isPromoting = true
        true
      case "normal" => chessState.from.y-chessState.to.y==0 && Math.abs(chessState.from.x-chessState.to.x)==1
    }
  }
  def pawnPosition(c: ChessState): String = {
    if(c.drawables(c.from.x)(c.from.y).asInstanceOf[Piece].side == "black"){
      c.from.x match {
        case 1 => "initial"
        case 6 => "willPromote"
        case _ => "normal"
      }
    }
    else{
      c.from.x match {
        case 6 => "initial"
        case 1 => "willPromote"
        case _ => "normal"
      }
    }
  }
  def verifyMoveToFront(c: ChessState): Boolean = {
    if(c.drawables(c.from.x)(c.from.y).asInstanceOf[Piece].side == "black"){
      c.from.x < c.to.x
    }
    else{
      c.from.x > c.to.x
    }
  }

  def applyPromotion(chessState: ChessState): Boolean = {
    var color = "white"
    if(chessState.turn % 2 != 0) {
      color = "black"
    }
    chessState.input match {
      case "q" => chessState.drawables(chessState.to.x)(chessState.to.y) =
        Piece("queen", color,chessState.to.x, chessState.to.y)
      case "r" => chessState.drawables(chessState.to.x)(chessState.to.y) =
        Piece("rook", color,chessState.to.x, chessState.to.y)
      case "b" => chessState.drawables(chessState.to.x)(chessState.to.y) =
        Piece("bishop", color,chessState.to.x, chessState.to.y)
      case "n" => chessState.drawables(chessState.to.x)(chessState.to.y) =
        Piece("knight", color,chessState.to.x, chessState.to.y)
    }
    chessState.turn += 1
    chessState.isPromoting = false
    true
  }
}

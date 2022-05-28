package Engine

import Games.Checkers.Checkers
import Games.Chess.Chess
import Games.TicTacToe.TicTacToe
import Games.Connect_4.Connect4

import java.awt.{Dimension, GridLayout}
import java.io.File
import javax.imageio.ImageIO
import javax.swing._

object StartWindow {
  def main(args: Array[String]): Unit = {
    val frame = new StartWindow
    frame.setVisible(true)
  }
}

class StartWindow() extends JFrame("Game Engine") {
  try{
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  }catch {
    case e: Exception => e.printStackTrace()
  }
  var gridLayout = new GridLayout(5,1,10,10)
  private val mainPanel = new JPanel(gridLayout)
  private val ticButton = new JButton("TicTacToe")
  private val connect4Button = new JButton("Connect 4")
  private val checkersButton = new JButton("Checkers")
  private val chessButton = new JButton("Chess")

  setResizable(false)
  setMinimumSize(new Dimension(200,200))
  setIconImage(ImageIO.read(new File("resources/icon.png")))

  mainPanel setBorder BorderFactory.createEmptyBorder(10,10,10,10)
  mainPanel add ticButton; mainPanel add connect4Button
  mainPanel add checkersButton; mainPanel add chessButton

  setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
  setContentPane(mainPanel)
  pack()

  ticButton addActionListener(_ => { new GameWindow("TicTacToe", TicTacToe.init(), TicTacToe.controller, TicTacToe.drawer); setVisible(false) })
  checkersButton addActionListener(_ => { new GameWindow("Checkers", Checkers.init(), Checkers.controller, Checkers.drawer); setVisible(false) })
  connect4Button addActionListener(e => { new GameWindow("Connect 4", Connect4.init(), Connect4.controller, Connect4.drawer); setVisible(false) })
  chessButton addActionListener(_ => { new GameWindow("Chess", Chess.init(), Chess.controller, Chess.drawer); setVisible(false) })
}

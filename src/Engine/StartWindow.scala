package Engine

import Games.Chess.Chess
import Games.TicTacToe.TicTacToe

import java.awt.{Dimension, GridLayout}
import javax.swing._

object StartWindow {
  def main(args: Array[String]): Unit = {
    val frame = new StartWindow
    frame.setVisible(true)
  }
}

class StartWindow() extends JFrame("Game Engine") {
  var gridLayout = new GridLayout(5,1,10,10)
  private val mainPanel = new JPanel(gridLayout)
  private val ticButton = new JButton("TicTacToe")
  private val connect4Button = new JButton("Connect 4")
  private val checkersButton = new JButton("Checkers")
  private val chessButton = new JButton("Chess")

  setResizable(false)
  setMinimumSize(new Dimension(200,200))

  mainPanel setBorder BorderFactory.createEmptyBorder(10,10,10,10)
  mainPanel add ticButton; mainPanel add connect4Button
  mainPanel add checkersButton; mainPanel add chessButton

  setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
  setContentPane(mainPanel)
  pack()

  ticButton addActionListener(_ => { new GameWindow("TicTacToe", TicTacToe.init(), TicTacToe.controller, TicTacToe.drawer); setVisible(false) })
//  connect4Button addActionListener(e => { new GameWindow("Connect 4"); setVisible(false) })
//  checkersButton addActionListener(e => { new GameWindow("Checkers"); setVisible(false) })
  chessButton addActionListener(_ => { new GameWindow("Chess", Chess.init(), Chess.controller, Chess.drawer); setVisible(false) })
}

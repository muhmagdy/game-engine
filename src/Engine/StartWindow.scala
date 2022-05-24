package Engine

import java.awt.event.{ActionEvent, ActionListener}
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
  setResizable(false)
  setMinimumSize(new Dimension(200,200))
  private var mainPanel = new JPanel(gridLayout)
  private var ticButton = new JButton("TicTacToe")
  private var connect4Button = new JButton("Connect 4")
  private var checkersButton = new JButton("Checkers")
  private var chessButton = new JButton("Chess")
  this.mainPanel setBorder BorderFactory.createEmptyBorder(10,10,10,10)
  this.mainPanel add ticButton; this.mainPanel add connect4Button;
  this.mainPanel add checkersButton; this.mainPanel add chessButton;
  this.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
  setContentPane(mainPanel)
  pack()
  ticButton.addActionListener(new ActionListener() {
    override def actionPerformed(e: ActionEvent): Unit = {
      new GameWindow("TicTacToe")
      setVisible(false)
    }
  })
  connect4Button addActionListener(e => new GameWindow("Connect 4"))

}

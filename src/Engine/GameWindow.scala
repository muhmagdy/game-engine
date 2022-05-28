package Engine

import java.awt.{BorderLayout, Dimension, FlowLayout, Font}
import javax.swing._
import java.awt.event.ActionListener

class GameWindow(val gameName: String,
                 val gameState: State,
                 val controller: State => Boolean,
                 val drawer: State => JPanel) extends JFrame(gameName) {

  /* Initializing Swing Components */
  private val turnLabel = new JLabel("Player 1 Turn", SwingConstants.CENTER)
  private val mainPanel = new JPanel()
  private val layoutManager = new BorderLayout()
  private val gamePanel = drawer(gameState)
  private val inputField = new JTextField()
  private val submitButton = new JButton("OK")

  /* Setting main panel options */
  setDefaultCloseOperation(3)
  setContentPane(mainPanel)
  mainPanel setLayout layoutManager
  mainPanel setBorder BorderFactory.createEmptyBorder(10, 10, 10, 10)

  inputField setPreferredSize new Dimension(450,25)
  turnLabel setFont(new Font("default", Font.BOLD, 25))

  private val lowerPanel = new JPanel()
  lowerPanel setLayout new FlowLayout(FlowLayout.CENTER)
  lowerPanel add inputField
  lowerPanel add submitButton

  private val middlePanel = new JPanel(new FlowLayout(FlowLayout.CENTER))
  middlePanel add gamePanel

  mainPanel add (turnLabel, BorderLayout.PAGE_START)
  mainPanel add (middlePanel, BorderLayout.CENTER)
  mainPanel add (lowerPanel, BorderLayout.PAGE_END)

  pack()
  setVisible(true)
  setResizable(false)

  val e: ActionListener = _ => { gameState.input = inputField.getText; update(); }

  submitButton addActionListener e
  inputField addActionListener e

  def update(): Unit = {
    if(controller(gameState)) {
      gameState.turn += 1
      gamePanel.repaint()
      gameState.turn = gameState.turn + 1
      if(gameState.turn % 2 == 0) turnLabel setText "Player 1 Turn"
      else turnLabel setText "Player 2 Turn"
    }
    inputField setText ""
  }
}

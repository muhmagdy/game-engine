import java.awt.{BorderLayout, Color, Dimension, FlowLayout, GridBagConstraints, GridBagLayout}
import javax.swing._

class GameWindow(val gameName: String) extends JFrame(gameName) {
  private val turnLabel = new JLabel("Player 1 Turn")
  private val mainPanel = new JPanel()
  private val layoutManager = new BorderLayout();
  private var gamePanel = new JPanel()
  private val inputField = new JTextField()
  private val submitButton = new JButton("OK")
  //Borders, Only for debugging
//  turnLabel setBorder(BorderFactory.createLineBorder(Color.black))
//  mainPanel setBorder(BorderFactory.createLineBorder(Color.black))
//  gamePanel setBorder(BorderFactory.createLineBorder(Color.black));
  mainPanel setLayout(layoutManager)
  gamePanel setPreferredSize new Dimension(500,500)
  inputField setPreferredSize new Dimension(450,25)
//  inputField setBorder(BorderFactory.createLineBorder(Color.black))
  setDefaultCloseOperation(3)
  setContentPane(mainPanel)
  mainPanel setBorder BorderFactory.createEmptyBorder(10, 10, 10, 10)
  private val lowerPanel = new JPanel();
  lowerPanel.setLayout( new FlowLayout(FlowLayout.CENTER));
  lowerPanel.add(inputField);
  lowerPanel.add(submitButton);
  private val middlePanel = new JPanel(new FlowLayout(FlowLayout.CENTER))
  middlePanel add gamePanel
  this.add(lowerPanel, BorderLayout.PAGE_END);
/*  layoutManager setConstraints (turnLabel, constraints)*/
  mainPanel add (turnLabel, BorderLayout.PAGE_START)
  mainPanel add (middlePanel, BorderLayout.CENTER)
  mainPanel add (lowerPanel, BorderLayout.PAGE_END)
//  mainPanel add (inputField, BorderLayout.PAGE_END)
//  mainPanel add (submitButton, BorderLayout.PAGE_END)
  pack()
//  setSize(800,800)
  setVisible(true)
  setResizable(false)
  println(gamePanel.getHeight)
  println(gamePanel.getWidth)
  middlePanel remove gamePanel
  middlePanel add TicTacToe.drawer(new State)
  //gamePanel.paint()
}

package Engine

import java.awt.{BorderLayout, Dimension, FlowLayout}
import javax.swing._
import Games.TicTacToe._
import Games.Chess._

class GameWindow(val gameName: String,
                 val s: State,
                 val controller: State => Boolean,
                 val drawer: State => JPanel) extends JFrame(gameName) {
  private val turnLabel = new JLabel("Player 1 Turn")
  private val mainPanel = new JPanel()
  private val layoutManager = new BorderLayout();
  private var gamePanel = drawer(s)
  private val inputField = new JTextField()
  private val submitButton = new JButton("OK")
  //Borders, Only for debugging
//  turnLabel setBorder(BorderFactory.createLineBorder(Color.black))
//  mainPanel setBorder(BorderFactory.createLineBorder(Color.black))
//  gamePanel setBorder(BorderFactory.createLineBorder(Color.black));
  mainPanel setLayout(layoutManager)
/*  gamePanel = */
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
//  println(gamePanel.getHeight)
//  println(gamePanel.getWidth)
  //middlePanel remove gamePanel
//  if(gameName == "TicTacToe") middlePanel add TicTacToe.drawer(new State)
  if(gameName == "Connect4") middlePanel add TicTacToe.drawer(new State)
  else if(gameName == "Checkers") middlePanel add TicTacToe.drawer(new State)
//  else if(gameName == "Chess") middlePanel add Chess.drawer(Chess.init())
  //gamePanel.paint()

  submitButton addActionListener(e => { s.input = inputField.getText; update();})

  def update(): Unit = {
    if(controller(s)) {
//      middlePanel remove gamePanel
//      gamePanel = drawer(s)
//      middlePanel add gamePanel
      gamePanel.repaint()
      if(s.turn % 2 == 0) turnLabel.setText("Player 1 Turn")
      else turnLabel.setText("Player 2 Turn")
    }
  }
}

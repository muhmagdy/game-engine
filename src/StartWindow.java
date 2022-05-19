//import javax.swing.*;
//import java.awt.event.ActionEvent;
//import java.awt.event.ActionListener;
//
//public class StartWindow extends JFrame {
//
//    private JPanel mainPanel;
//
//    private JButton ticButton;
//    private JButton connect4Button;
//    private JButton checkersButton;
//    private JButton chessButton;
//
//    StartWindow(){
//        super("Game Engine");
//        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
//        setContentPane(mainPanel);
//        pack();
//
//        ticButton.addActionListener(new ActionListener() {
//            @Override
//            public void actionPerformed(ActionEvent e) {
//                new GameWindow("Tic Tac Toe");
//                setVisible(false);
//            }
//        });
//    }
//
//    public static void main(String[] args){
//        JFrame frame = new StartWindow();
//        frame.setVisible(true);
//    }
//}

package it.unipi.puzzle;

import javax.swing.*;
import java.awt.*;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 *
 * @author Alessandro Scala
 */
public class EightBoard extends javax.swing.JFrame {

	private final java.util.List<RestartListener> restartListeners = new ArrayList<>();
	
	public EightBoard() {
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		try {
			for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
				if ("Nimbus".equals(info.getName())) {
					javax.swing.UIManager.setLookAndFeel(info.getClassName());
					break;
				}
			}
		} catch (ClassNotFoundException | InstantiationException | IllegalAccessException |
		         UnsupportedLookAndFeelException ignored) {}
		
		
		JPanel boardPanel = new JPanel();
		GridLayout boardLayout = new GridLayout(3, 3);
		boardPanel.setLayout(boardLayout);

		EightController controllerLabel = new EightController();
		JLabel tiles[] = new JLabel[9];
		for (int i = 1; i <= 9; i++) {
			tiles[i-1] = new EightTile(i);
			setupComponentLayout(tiles[i-1], true);
			boardPanel.add("tile" + i, tiles[i-1]);
			this.addRestartListener((EightTile)tiles[i-1]);
			tiles[i-1].addVetoableChangeListener(controllerLabel);
			((EightTile)tiles[i-1]).flipDelegate.ifPresent(controllerLabel::addFlipListener);
		}

		for (int i = 0; i < 9; i++) {
			for (int j = 0; j < 9; j++) {
				if(i != j){
					tiles[i].addPropertyChangeListener((PropertyChangeListener) tiles[j]);
				}
			}
		}
		
		JPanel controlPanel = new JPanel();
		GridLayout controlLayout = new GridLayout(0, 3);
		controlPanel.setLayout(controlLayout);

		JButton restartButton = new JButton("Restart");
		JButton flipButton = new JButton("Flip");
		controlPanel.add(controllerLabel);
		controlPanel.add(restartButton);
		controlPanel.add(flipButton);
		
		Consumer<JButton> configureButton = (b) -> {
			b.setMargin(new Insets(0, 0, 0, 0));
			b.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		};
		
		setupComponentLayout(controllerLabel, false);
		setupComponentLayout(restartButton, false);
		setupComponentLayout(flipButton, false);
		configureButton.accept(restartButton);
		configureButton.accept(flipButton);
		restartButton.setBackground(Color.orange);
		flipButton.setBackground(Color.cyan);
		
		boardPanel.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		controlPanel.setBorder(BorderFactory.createEmptyBorder(0,10,10,10));
		Container contentPane = this.getContentPane();
		contentPane.add(boardPanel, BorderLayout.NORTH);
		contentPane.add(controlPanel, BorderLayout.SOUTH);

		restartButton.addActionListener((e) -> this.fireRestartEvent());
		this.addRestartListener(controllerLabel);
		flipButton.addActionListener(controllerLabel::flipTiles);

		this.pack();
		this.setResizable(false);
		centerFrame(this);
		this.fireRestartEvent();
	}

	public void fireRestartEvent() {
		List<Integer> labels = Arrays.stream(new Integer[]{1, 2, 3, 4, 5, 6, 7, 8, 9}).collect(Collectors.toCollection(ArrayList::new));
		Collections.shuffle(labels);
		restartListeners.forEach(listener -> listener.restart(labels));
	}

	public void addRestartListener(RestartListener listener){
		this.restartListeners.add(listener);
	}

	public void removeRestartListener(RestartListener listener){
		this.restartListeners.remove(listener);
	}
	
	public static void setupComponentLayout(JComponent comp, boolean isTile){
		comp.setOpaque(true);
		if(comp instanceof JLabel) {
			((JLabel) comp).setHorizontalAlignment(JLabel.CENTER);
			((JLabel) comp).setVerticalAlignment(JLabel.CENTER);
		}
		comp.setFont(new Font("Helvetica Neue", Font.PLAIN, isTile ? 24 : 18));
		comp.setBorder(BorderFactory.createCompoundBorder(
				BorderFactory.createLineBorder(Color.black),
				isTile ? BorderFactory.createEmptyBorder(25,10,25,10) : BorderFactory.createEmptyBorder(10,10,10,10)
		));
	}
	
	/**
	 * Moves the frame to the center of the screen
	 */
	public static void centerFrame(JFrame frame){
		Dimension dimension = Toolkit.getDefaultToolkit().getScreenSize();
		frame.setLocation((int) (dimension.getWidth() / 2 - frame.getSize().getWidth() / 2), (int) (dimension.getHeight() / 2 - frame.getSize().getHeight() / 2));
	}
}

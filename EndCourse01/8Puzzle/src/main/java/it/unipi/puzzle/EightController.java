/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Beans/Bean.java to edit this template
 */
package it.unipi.puzzle;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.beans.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * @author Alessandro Scala
 */
public class EightController extends JLabel implements Serializable, RestartListener, VetoableChangeListener {
	private static Color OK_COLOR;
	private static final Color KO_COLOR = Color.RED;
	private final Map<Integer, Integer> labelsToPositions = new HashMap<>(9);
	private final List<FlipListener> flipListeners = new ArrayList<>(2);
	public EightController() {
		super("OK");
		EightController.OK_COLOR = this.getBackground();
	}

	public void flipTiles(ActionEvent actionEvent) {
		if(labelsToPositions.get(9) == 9) {
			int label1 = getKeyFromValue(1);
			int label2 = getKeyFromValue(2);
			fireFlipEvent(label1, label2);
			this.labelsToPositions.put(label2, 1);
			this.labelsToPositions.put(label1, 2);
		}
	}

	@Override
	public void vetoableChange(PropertyChangeEvent evt) throws PropertyVetoException {
		if(evt.getPropertyName().equals("label")){
			int oldPosition = labelsToPositions.get((int)evt.getOldValue());
			int newPosition = labelsToPositions.get((int)evt.getNewValue());
			if(manhattanDistance(oldPosition, newPosition) != 1){
				onTileSwapVetoed();
				throw new PropertyVetoException("Cannot swap the two labels", evt);
			}else{
				this.setText("OK");
				labelsToPositions.put((int)evt.getOldValue(), newPosition);
				labelsToPositions.put((int)evt.getNewValue(), oldPosition);
			}
		}
	}

	private static int manhattanDistance(int from, int to){
		// Decrementing the two to convert from the 1-based representation to a more convenient 0-based one
		from--;
		to--;
		return Math.abs((from % 3) - (to % 3)) + Math.abs((from / 3) - (to / 3));
	}

	@Override
	public void restart(List<Integer> labels) {
		for(int i = 0; i < 9; i++){
			labelsToPositions.put(labels.get(i), i + 1);
		}
	}

	private void onTileSwapVetoed(){
		this.setText("KO");
		this.setBackground(KO_COLOR);
		javax.swing.Timer timer = new Timer(500, e -> {
			this.setBackground(OK_COLOR);
		});
		timer.setRepeats(false);
		timer.start();
	}

	public void addFlipListener(FlipListener listener){
		this.flipListeners.add(listener);
	}

	public void removeFlipListener(FlipListener listener){
		this.flipListeners.remove(listener);
	}

	private void fireFlipEvent(int label1, int label2){
		this.flipListeners.forEach(l -> {l.flip(label1, label2);});
	}

	private int getKeyFromValue(int key){ // Very ugly, but it avoids the need to keep track of the labels of the two tiles
		return labelsToPositions.entrySet()
				.stream()
				.filter(entry -> entry.getValue() == key)
				.map(Map.Entry::getKey)
				.findFirst().get();
	}
}

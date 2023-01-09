/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Beans/Bean.java to edit this template
 */
package it.unipi.puzzle;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.Serializable;
import java.beans.*;
import java.util.Optional;

/**
 *
 * @author Alessandro Scala
 */
public class EightTile extends JLabel implements Serializable, RestartListener, PropertyChangeListener {
	private final static Color CORRECT_COLOR = Color.green;
	private final static Color WRONG_COLOR = Color.yellow;
	private final static Color ERROR_COLOR = Color.RED;
	private int position;
	private int label;

	public Optional<FlipListener> flipDelegate = Optional.empty();

	public EightTile(){
		this(0);
	}

	public EightTile(int position) {
		super(Integer.toString(position));
		
		this.position = position;
		updateLabel(position);

		if(this.position == 1 || this.position == 2){
			this.flipDelegate = Optional.of((label1, label2) -> this.updateLabel(this.position == 1 ? label2 : label1));
		}

		this.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				setLabel(9);
			}
		});
	}

	public void setLabel(int label){
		int oldValue = this.label;
		try{
			this.fireVetoableChange("label", oldValue, label);
			updateLabel(label);
			this.firePropertyChange("label", oldValue, label);
		}catch (PropertyVetoException ignored) {
			onLabelChangeVetoed();
		}
	}

	public int getLabel(){
		return this.label;
	}

	public void setPosition(int position){
		this.position = position;
	}

	public int getPosition(){
		return this.position;
	}

	private void updateLabel(int newLabel){
		this.label = newLabel;
		setColor();
		if(label == 9){
			this.setBackground(Color.gray);
			this.setText("");
			return;
		}
		
		this.setText(Integer.toString(label));

	}

	private void setColor(){
		if(label == 9){
			this.setBackground(Color.gray);
			return;
		}
		if(label == position){
			this.setBackground(CORRECT_COLOR);
		}else{
			this.setBackground(WRONG_COLOR);
		}
	}

	public void restart(java.util.List<Integer> labels){
		updateLabel(labels.get(this.position-1));
	}

	@Override
	public void propertyChange(PropertyChangeEvent evt) {
		if(evt.getPropertyName().equals("label")){
			if(this.label == 9){
				this.updateLabel((int)evt.getOldValue());
			}
		}
	}
	private void onLabelChangeVetoed(){
		this.setBackground(ERROR_COLOR);
		javax.swing.Timer timer = new Timer(500, e -> {
			this.setColor();
		});
		timer.setRepeats(false);
		timer.start();
	}
}
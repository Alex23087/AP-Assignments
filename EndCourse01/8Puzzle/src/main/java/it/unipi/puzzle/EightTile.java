/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Beans/Bean.java to edit this template
 */
package it.unipi.puzzle;

import javax.swing.*;
import java.awt.*;
import java.io.Serializable;

/**
 *
 * @author Alessandro Scala
 */
public class EightTile extends JLabel implements Serializable {
	private final Color CORRECT_COLOR = Color.green;
	private final Color WRONG_COLOR = Color.yellow;
	private int position;
	private int label;
	public EightTile(int position) {
		super(Integer.toString(position));
		
		this.position = position;
		updateLabel(position);
	}

	private void updateLabel(int newLabel){
		this.label = newLabel;
		if(label == 9){
			this.setBackground(Color.gray);
			this.setText("");
			return;
		}
		
		this.setText(Integer.toString(label));
		if(label == position){
			this.setBackground(CORRECT_COLOR);
		}else{
			this.setBackground(WRONG_COLOR);
		}
	}
}
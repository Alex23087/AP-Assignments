/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Project/Maven2/JavaApp/src/main/java/${packagePath}/${mainClassName}.java to edit this template
 */

package it.unipi.puzzle;

/**
 *
 * @author Alessandro Scala
 */
public class App {
	
	public static void main(String[] args) {
		System.setProperty("awt.useSystemAAFontSettings","on");
		new EightBoard().setVisible(true);
	}
}

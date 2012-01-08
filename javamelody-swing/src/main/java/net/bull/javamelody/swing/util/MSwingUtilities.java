/*
 * Copyright 2008-2012 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody.swing.util;

import java.awt.AWTEvent;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.AWTEventListener;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.PrintStream;

import javax.swing.FocusManager;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.WindowConstants;

/**
 * Classe utilitaire pour Swing.
 * @author Emeric Vernat
 */
public final class MSwingUtilities {
	private MSwingUtilities() {
		super();
	}

	/**
	 * Affiche la trace de l'exception dans la console d'erreur et affiche une boîte de dialogue pour afficher l'exception.
	 * @param throwable Throwable
	 */
	public static void showException(Throwable throwable) {
		throwable.printStackTrace(getSystemErrorStream());
		JOptionPane.showMessageDialog(null, throwable.toString(),
				UIManager.getString("OptionPane.messageDialogTitle"), JOptionPane.ERROR_MESSAGE);
		// on pourrait affichage une boîte de dialogue plus évoluée pour permettre d'afficher la stack trace en détail
	}

	/**
	 * Affiche une boîte de dialogue de confirmation.
	 * @param component Component
	 * @param message String
	 * @return boolean
	 */
	public static boolean showConfirmation(Component component, String message) {
		return JOptionPane.showConfirmDialog(SwingUtilities.getWindowAncestor(component), message,
				UIManager.getString("OptionPane.titleText"), JOptionPane.OK_OPTION
						| JOptionPane.CANCEL_OPTION) == JOptionPane.OK_OPTION;
	}

	/**
	 * Affiche une boîte de dialogue d'information.
	 * @param component Component
	 * @param message String
	 */
	public static void showMessage(Component component, String message) {
		JOptionPane.showMessageDialog(SwingUtilities.getWindowAncestor(component), message,
				UIManager.getString("OptionPane.messageDialogTitle"),
				JOptionPane.INFORMATION_MESSAGE);
	}

	/**
	 * Retourne l'instance courante de la classe componentClass contenant l'élément component. <br/>
	 * Cette méthode peut-être très utile pour récupérer une référence à un parent éloigné (ancêtre), en l'absence de référence directe du type attribut.
	 *
	 * <br/>
	 * Ex : un composant panel désire une référence sur sa JFrame parente, alors l'instruction suivante suffit : getAncestorOfClass(JFrame.class, panel)
	 *
	 * @return Component
	 * @param <T>
	 *           le type du composant recherché
	 * @param componentClass
	 *           Class
	 * @param component
	 *           Component
	 */
	@SuppressWarnings("unchecked")
	public static <T> T getAncestorOfClass(final Class<T> componentClass, final Component component) {
		return (T) SwingUtilities.getAncestorOfClass(componentClass, component);
	}

	/**
	 * Retourne le focusOwner permanent.<br/>
	 * Le focusOwner permanent est défini comme le dernier Component à avoir reçu un événement FOCUS_GAINED permanent.<br/>
	 * Le focusOwner et le focusOwner permanent sont équivalent sauf si un changement temporaire de focus<br/>
	 * est en cours. Si c'est le cas, le focusOwner permanent redeviendra &galement<br/>
	 * le focusOwner à la fin de ce changement de focus temporaire.
	 *
	 * @return Component
	 */
	public static Component getPermanentFocusOwner() {
		// return new DefaultKeyboardFocusManager().getPermanentFocusOwner();
		return FocusManager.getCurrentManager().getPermanentFocusOwner();
	}

	/**
	 * Retourne la fenêtre possédant le focus.
	 *
	 * @return Component
	 */
	public static Window getFocusedWindow() {
		// return new DefaultKeyboardFocusManager().getFocusedWindow();
		return FocusManager.getCurrentManager().getFocusedWindow();
	}

	/**
	 * Démarre un composant dans une Frame (utile pour écrire des méthodes main sur des panels en développement).
	 *
	 * @param component
	 *           JComponent
	 * @return la Frame créée
	 */
	public static JFrame run(final JComponent component) {
		final JFrame frame = new JFrame();
		try {
			frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
			component.setOpaque(true); // si opaque false, il y a des pbs de paint
			frame.setContentPane(component);
			frame.setTitle(component.getClass().getName());
			frame.pack();
			frame.setLocationRelativeTo(null);
			frame.setVisible(true);
		} catch (final Exception exception) {
			exception.printStackTrace(getSystemErrorStream()); // NOPMD
		}
		return frame;
	}

	/**
	 * @return System.err
	 */
	private static PrintStream getSystemErrorStream() {
		return System.err;
	}

	/**
	 * Démarre un composant dans une Frame sans pack() (utile pour écrire des méthodes main sur des panels en développement).
	 *
	 * @param component
	 *           JComponent
	 * @return la Frame créée
	 */
	public static JFrame runUnpacked(final JComponent component) {
		component.setPreferredSize(component.getSize());
		return run(component);
	}

	/**
	 * Initialisation d'événements sur la touche Escape pour fermer les dialogues.
	 */
	public static void initEscapeClosesDialogs() {
		final AWTEventListener awtEventListener = new AWTEventListener() {
			/** {@inheritDoc} */
			@Override
			public void eventDispatched(final AWTEvent event) {
				if (event instanceof KeyEvent
						&& ((KeyEvent) event).getKeyCode() == KeyEvent.VK_ESCAPE
						&& event.getID() == KeyEvent.KEY_PRESSED) {
					escapePressed();
				}
			}
		};
		Toolkit.getDefaultToolkit().addAWTEventListener(awtEventListener, AWTEvent.KEY_EVENT_MASK);
	}

	/**
	 * La touche Esc a été pressée : fermer la dialogue modale ouverte.
	 */
	static void escapePressed() {
		final Component focusOwner = getPermanentFocusOwner();
		final Window focusedWindow = SwingUtilities.getWindowAncestor(focusOwner);
		if (focusedWindow instanceof Dialog && ((Dialog) focusedWindow).isModal()) {
			//			try {
			//				final Robot robot = new Robot();
			//				robot.keyPress(KeyEvent.VK_ALT);
			//				robot.keyPress(KeyEvent.VK_F4);
			//				robot.keyRelease(KeyEvent.VK_F4);
			//				robot.keyRelease(KeyEvent.VK_ALT);
			//			} catch (final AWTException e) {
			//				throw new IllegalStateException(e);
			//			}
			focusedWindow.dispose();
		}
	}

	/**
	 * Redimensionne une ImageIcon.
	 * @param icon ImageIcon
	 * @param targetWidth int
	 * @param targetHeight int
	 * @return ImageIcon
	 */
	public static ImageIcon getScaledInstance(ImageIcon icon, int targetWidth, int targetHeight) {
		return new ImageIcon(getScaledInstance(icon.getImage(), targetWidth, targetHeight));
	}

	/**
	 * Redimensionne une image.
	 * @param img Image
	 * @param targetWidth int
	 * @param targetHeight int
	 * @return Image
	 */
	public static Image getScaledInstance(Image img, int targetWidth, int targetHeight) {
		final int type = BufferedImage.TYPE_INT_ARGB;
		Image ret = img;
		final int width = ret.getWidth(null);
		final int height = ret.getHeight(null);
		if (width != targetWidth && height != targetHeight) {
			// a priori plus performant que Image.getScaledInstance
			final BufferedImage scratchImage = new BufferedImage(targetWidth, targetHeight, type);
			final Graphics2D g2 = scratchImage.createGraphics();
			g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
					RenderingHints.VALUE_INTERPOLATION_BILINEAR);
			g2.drawImage(ret, 0, 0, targetWidth, targetHeight, 0, 0, width, height, null);
			g2.dispose();
			ret = scratchImage;
		}
		return ret;
	}
}

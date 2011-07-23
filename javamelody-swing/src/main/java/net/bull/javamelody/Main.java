/*
 * Copyright 2008-2010 by Emeric Vernat
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
package net.bull.javamelody;

import java.awt.Frame;
import java.io.IOException;
import java.net.URL;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import net.bull.javamelody.util.MSwingUtilities;
import net.bull.javamelody.util.ShadowPopupFactory;

/**
 * Classe Main.
 * @author Emeric Vernat
 */
public final class Main {
	private Main() {
		super();
	}

	/**
	 * Méthode main.
	 * @param args Arguments du programme
	 */
	public static void main(String[] args) {
		initLookAndFeel();
		ShadowPopupFactory.install();
		MSwingUtilities.initEscapeClosesDialogs();

		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				try {
					// affichage de la frame dans le thread AWT
					showFrame();
				} catch (final Throwable t) { // NOPMD
					MSwingUtilities.showException(t);
					System.exit(1); // NOPMD
				}
			}
		});
	}

	static void showFrame() throws IOException {
		final JFrame frame = new JFrame();
		frame.setTitle("Java Melody");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		// TODO mettre les instances de MainPanel dans des onglets
		final CollectorServer collectorServer = new CollectorServer();
		try {
			// on attend que les données de test soient chargées
			Thread.sleep(1500);
		} catch (final InterruptedException e) {
			throw new RuntimeException(e); // NOPMD
		}
		final Collector collector = collectorServer.getCollectorByApplication("test");
		final List<JavaInformations> javaInformationsList = collectorServer
				.getJavaInformationsByApplication("test");
		final String collectorUrl = CollectorServer.getUrlsByApplication("test").get(0)
				.toExternalForm();
		final URL monitoringUrl = new URL(collectorUrl.substring(0, collectorUrl.indexOf('?')));
		final MainPanel contentPane = new MainPanel(collector, javaInformationsList, monitoringUrl);
		frame.setContentPane(contentPane);
		frame.setIconImage(ImageIconCache.getImageIcon("systemmonitor.png").getImage());
		// définit la taille
		frame.pack();
		frame.setLocationRelativeTo(null);
		frame.setExtendedState(Frame.MAXIMIZED_BOTH);
		frame.setVisible(true);
	}

	/**
	 * Initialisation du L&F.
	 */
	private static void initLookAndFeel() {
		// UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel");
		for (final LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
			if ("Nimbus".equals(info.getName())) {
				try {
					UIManager.setLookAndFeel(info.getClassName());
					break;
				} catch (final Exception e) {
					throw new RuntimeException(e); // NOPMD
				}
			}
		}
	}
}

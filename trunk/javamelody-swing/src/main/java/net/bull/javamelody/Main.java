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
package net.bull.javamelody;

import java.awt.Color;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.lang.Thread.UncaughtExceptionHandler;
import java.net.URL;
import java.util.Arrays;
import java.util.List;

import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import net.bull.javamelody.swing.util.MSwingUtilities;
import net.bull.javamelody.swing.util.ShadowPopupFactory;

import org.apache.log4j.Logger;

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
		log("starting");

		initLookAndFeel();
		// une touche bleu-clair pour avoir une teinte moins grisatre
		UIManager.put("control", new Color(225, 225, 250));

		ShadowPopupFactory.install();
		MSwingUtilities.initEscapeClosesDialogs();
		// on définit le répertoire courant, car par exemple dans JavaWebStart il n'est pas bon par défaut
		System.setProperty("user.dir", System.getProperty("user.home"));

		Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler() {
			@Override
			public void uncaughtException(Thread t, Throwable e) {
				MSwingUtilities.showException(e);
			}
		});

		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				try {
					// affichage de la frame dans le thread AWT
					showFrame();
				} catch (final Throwable t) { // NOPMD
					MSwingUtilities.showException(t);
					exit();
				}
			}
		});
	}

	static void showFrame() throws IOException {
		final String application = System.getProperty("javamelody.application");
		final String url = System.getProperty("javamelody.url");
		final String range = System.getProperty("javamelody.range");
		if (application == null || url == null || range == null) {
			throw new IllegalStateException(
					"There must be 3 system properties: javamelody.application, javamelody.url, javamelody.range");
		}
		final String locale = System.getProperty("javamelody.locale");
		if (locale != null) {
			System.setProperty("user.language", locale);
		}
		final boolean collectorServer = Boolean.parseBoolean(System
				.getProperty("javamelody.collectorServer"));
		final List<URL> urls = Arrays.asList(new URL(url));
		final Range selectedRange = Range.parse(range);
		log("Monitoring of " + application + " on " + url);
		log("creating frame");
		final RemoteCollector remoteCollector = new RemoteCollector(application, urls);
		remoteCollector.setCookies(System.getProperty("cookies"));
		final MainPanel contentPane = new MainPanel(remoteCollector, selectedRange, collectorServer);
		final MainFrame frame = new MainFrame();
		frame.setTitle(I18N.getFormattedString("Monitoring_sur", application));
		frame.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				log("closing");
			}
		});
		frame.setContentPane(contentPane);
		frame.setVisible(true);
		log("frame displayed");
	}

	static void log(String message) {
		Logger.getLogger(Main.class).info(message);
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

	@SuppressWarnings("all")
	static void exit() {
		System.exit(1); // NOPMD
	}
}

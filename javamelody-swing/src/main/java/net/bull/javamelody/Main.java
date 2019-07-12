/*
 * Copyright 2008-2019 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody;

import java.awt.Color;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.lang.Thread.UncaughtExceptionHandler;
import java.net.URL;
import java.text.DateFormat;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import org.apache.log4j.Logger;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.Range;
import net.bull.javamelody.internal.model.RemoteCollector;
import net.bull.javamelody.internal.web.JnlpPage;
import net.bull.javamelody.swing.util.MSwingUtilities;
import net.bull.javamelody.swing.util.ShadowPopupFactory;

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

		// jnlp prefix to fix:
		// http://stackoverflow.com/questions/19400725/with-java-update-7-45-the-system-properties-no-more-set-from-jnlp-tag-property
		// https://bugs.openjdk.java.net/browse/JDK-8023821
		for (final Object property : Collections.list(System.getProperties().keys())) {
			final String name = String.valueOf(property);
			if (name.startsWith(JnlpPage.JNLP_PREFIX)) {
				final String value = System.getProperty(name);
				System.setProperty(name.substring(JnlpPage.JNLP_PREFIX.length()), value);
			}
		}

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
		final boolean collectorServer = Boolean
				.parseBoolean(System.getProperty("javamelody.collectorServer"));
		final List<URL> urls = Arrays.asList(new URL(url));
		final DateFormat dateFormat = I18N.createDateFormat();
		final Range selectedRange = Range.parse(range, dateFormat);
		log("Monitoring of " + application + " on " + url);
		log("creating frame");
		final RemoteCollector remoteCollector = new RemoteCollector(application, urls);
		remoteCollector.setCookies(System.getProperty("cookies"));
		final MainPanel contentPane = new MainPanel(remoteCollector, selectedRange,
				collectorServer);
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
		// UIManager.setLookAndFeel("javax.swing.plaf.nimbus.NimbusLookAndFeel");
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

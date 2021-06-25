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
package net.bull.javamelody.internal.model;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Scanner;
import java.util.regex.Pattern;

/**
 * Liste et informations sur les process linux ou unix.
 * @author Emeric Vernat
 */
public final class ProcessInformations implements Serializable {
	private static final long serialVersionUID = 2163916067335213382L;
	private static final Pattern WINDOWS_STATE_PATTERN = Pattern.compile("................");
	private static final Pattern WINDOWS_CPU_TIME_PATTERN = Pattern.compile("[0-9:]*");

	private final String user;
	private final int pid;
	// rapport Temps CPU / Temps utilisateur.
	// Il s'agit d'un rapport entre le temps d'exécution effectif et le temps depuis lequel le processus a été lancé
	private final float cpuPercentage;
	private final float memPercentage;
	// taille virtuelle de l'image du processus (code + données + pile).
	private final int vsz;
	// taille résidente de l'image du processus. Nombre de kilo-octets se trouvant en mémoire
	private final int rss;
	// numéro mineur de périphérique tty (terminal de contrôle)
	private final String tty;
	// état du processus.  Le premier champ correspond à
	// R (runnable) prêt à  être exécuté,
	// S (sleeping) endormi,
	// D sommeil ininterruptible,
	// T (traced) arrêté ou suivi,
	// Z (zombie).
	// Le second champ contient W si le processus n'a pas de pages résidentes.
	// Le troisième champ contient N si le processus a une valeur de gentillesse positive (nice, champ NI).
	private final String stat;
	private final String start;
	private final String cpuTime;
	private final String command;

	private ProcessInformations(Scanner sc, boolean windows, boolean macOrAix) {
		super();
		if (windows) {
			final StringBuilder imageNameBuilder = new StringBuilder(sc.next());
			while (!sc.hasNextInt()) {
				imageNameBuilder.append(' ').append(sc.next());
			}
			pid = sc.nextInt();
			if ("Console".equals(sc.next())) {
				// ce if est nécessaire si windows server 2003 mais sans connexion à distance ouverte
				// (comme tasklist.txt dans resources de test,
				// car parfois "Console" est présent mais parfois non)
				sc.next();
			}
			final String memory = sc.next();
			cpuPercentage = -1;
			memPercentage = -1;
			vsz = Integer.parseInt(memory.replace(".", "").replace(",", "").replace("ÿ", ""));
			rss = -1;
			tty = null;
			sc.next();
			sc.skip(WINDOWS_STATE_PATTERN);
			stat = null;
			final StringBuilder userBuilder = new StringBuilder(sc.next());
			while (!sc.hasNext(WINDOWS_CPU_TIME_PATTERN)) {
				userBuilder.append(' ').append(sc.next());
			}
			this.user = userBuilder.toString();
			start = null;
			cpuTime = sc.next();
			command = imageNameBuilder.append("   (").append(sc.nextLine().trim()).append(')')
					.toString();
		} else {
			user = sc.next();
			pid = sc.nextInt();
			cpuPercentage = Float.parseFloat(sc.next().replace(",", "."));
			memPercentage = Float.parseFloat(sc.next().replace(",", "."));
			vsz = sc.nextInt();
			rss = sc.nextInt();
			tty = sc.next();
			stat = sc.next();
			if (macOrAix && sc.hasNextInt()) {
				start = sc.next() + ' ' + sc.next();
			} else {
				start = sc.next();
			}
			cpuTime = sc.next();
			command = sc.nextLine();
		}
	}

	public static List<ProcessInformations> buildProcessInformations(InputStream in,
			boolean windows, boolean macOrAix) {
		final String charset;
		if (windows) {
			charset = "Cp1252";
		} else {
			charset = "UTF-8";
		}
		final Scanner sc = new Scanner(in, charset);
		sc.useRadix(10);
		sc.useLocale(Locale.US);
		sc.nextLine();
		if (windows) {
			sc.nextLine();
			sc.nextLine();
		}

		final List<ProcessInformations> processInfos = new ArrayList<>();
		while (sc.hasNext()) {
			final ProcessInformations processInfo = new ProcessInformations(sc, windows, macOrAix);
			processInfos.add(processInfo);
		}
		return Collections.unmodifiableList(processInfos);
	}

	public static List<ProcessInformations> buildProcessInformations() throws IOException {
		Process process = null;
		try {
			// pour nodes Jenkins, on évalue ces propriétés à chaque fois sans utiliser de constantes
			final String osName = System.getProperty("os.name").toLowerCase(Locale.ENGLISH);
			final boolean windows = osName.contains("windows");
			final boolean mac = osName.contains("mac");
			final boolean aix = osName.contains("aix");
			if (windows) {
				process = Runtime.getRuntime().exec(new String[] { "cmd", "/c", "tasklist /V" });
			} else if (mac || aix) {
				// le "f" de "ps wauxf" n'est pas supporté sur Mac OS X et sur AIX, cf issues 74 et 99
				process = Runtime.getRuntime().exec(new String[] { "/bin/sh", "-c", "ps waux" });
			} else {
				// tous les systèmes (ou presque) non Windows sont une variante de linux ou unix
				// (http://mindprod.com/jgloss/properties.html) qui acceptent la commande ps
				process = Runtime.getRuntime().exec(new String[] { "/bin/sh", "-c", "ps wauxf" });
			}
			return buildProcessInformations(process.getInputStream(), windows, mac || aix);
		} finally {
			if (process != null) {
				// évitons http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6462165
				process.getInputStream().close();
				process.getOutputStream().close();
				process.getErrorStream().close();
				process.destroy();
			}
		}
	}

	public String getUser() {
		return user;
	}

	public int getPid() {
		return pid;
	}

	public float getCpuPercentage() {
		return cpuPercentage;
	}

	public float getMemPercentage() {
		return memPercentage;
	}

	public int getVsz() {
		return vsz;
	}

	public int getRss() {
		return rss;
	}

	public String getTty() {
		return tty;
	}

	public String getStat() {
		return stat;
	}

	public String getStart() {
		return start;
	}

	public String getCpuTime() {
		return cpuTime;
	}

	public String getCommand() {
		return command;
	}
}

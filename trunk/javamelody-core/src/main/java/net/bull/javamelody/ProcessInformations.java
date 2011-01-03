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
final class ProcessInformations implements Serializable {
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

	private ProcessInformations(Scanner sc, boolean windows) {
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
			cpuPercentage = sc.nextFloat();
			memPercentage = sc.nextFloat();
			vsz = sc.nextInt();
			rss = sc.nextInt();
			tty = sc.next();
			stat = sc.next();
			start = sc.next();
			cpuTime = sc.next();
			command = sc.nextLine();
		}
	}

	static List<ProcessInformations> buildProcessInformations(InputStream in, boolean windows) {
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

		final List<ProcessInformations> processInfos = new ArrayList<ProcessInformations>();
		while (sc.hasNext()) {
			final ProcessInformations processInfo = new ProcessInformations(sc, windows);
			processInfos.add(processInfo);
		}
		return Collections.unmodifiableList(processInfos);
	}

	static List<ProcessInformations> buildProcessInformations() throws IOException {
		Process process = null;
		try {
			// pour nodes hudson, on évalue ces propriétés à chaque fois sans utiliser de constantes
			final String osName = System.getProperty("os.name").toLowerCase(Locale.getDefault());
			final boolean windows = osName.contains("windows");
			final boolean mac = osName.contains("mac");
			if (windows) {
				process = Runtime.getRuntime().exec(new String[] { "cmd", "/c", "tasklist /V" });
			} else if (mac) {
				// le "f" de "ps wauxf" n'est pas supporté sur Mac OS X, cf issue 74
				process = Runtime.getRuntime().exec(new String[] { "/bin/sh", "-c", "ps waux" });
			} else {
				// tous les systèmes (ou presque) non Windows sont une variante de linux ou unix
				// (http://mindprod.com/jgloss/properties.html) qui acceptent la commande ps
				process = Runtime.getRuntime().exec(new String[] { "/bin/sh", "-c", "ps wauxf" });
			}
			return buildProcessInformations(process.getInputStream(), windows);
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

	String getUser() {
		return user;
	}

	int getPid() {
		return pid;
	}

	float getCpuPercentage() {
		return cpuPercentage;
	}

	float getMemPercentage() {
		return memPercentage;
	}

	int getVsz() {
		return vsz;
	}

	int getRss() {
		return rss;
	}

	String getTty() {
		return tty;
	}

	String getStat() {
		return stat;
	}

	String getStart() {
		return start;
	}

	String getCpuTime() {
		return cpuTime;
	}

	String getCommand() {
		return command;
	}
}

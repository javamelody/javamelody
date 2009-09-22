package net.bull.javamelody;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Locale;
import java.util.StringTokenizer;

/**
 * @author (getPID) Santhosh Kumar T, http://code.google.com/p/jlibs/, licence LGPL
 * @author (getpids.exe) Daniel Scheibli, http://www.scheibli.com/projects/getpids/index.html, licence GPL
 */
final class PID {
	private PID() {
		super();
	}

	/**
	 * @return PID du process java
	 */
	static String getPID() {
		String pid = System.getProperty("pid");
		if (pid == null) {
			final String[] cmd;
			File tempFile = null;
			Process process = null;
			try {
				try {
					if (!System.getProperty("os.name").toLowerCase(Locale.getDefault()).contains(
							"windows")) {
						cmd = new String[] { "/bin/sh", "-c", "echo $$ $PPID" };
					} else {
						// getpids.exe is taken from http://www.scheibli.com/projects/getpids/index.html (GPL)
						tempFile = File.createTempFile("getpids", ".exe");

						// extract the embedded getpids.exe file from the jar and save it to above file
						pump(PID.class.getResourceAsStream("resource/getpids.exe"),
								new FileOutputStream(tempFile), true, true);
						cmd = new String[] { tempFile.getAbsolutePath() };
					}
					process = Runtime.getRuntime().exec(cmd);
					final ByteArrayOutputStream bout = new ByteArrayOutputStream();
					pump(process.getInputStream(), bout, false, true);

					final StringTokenizer stok = new StringTokenizer(bout.toString());
					stok.nextToken(); // this is pid of the process we spanned
					pid = stok.nextToken();
					System.setProperty("pid", pid);

					// waitFor nécessaire sous windows server 2003
					// (sinon le fichier temporaire getpidsxxx.exe n'est pas effacé)
					process.waitFor();
				} finally {
					if (process != null) {
						// évitons http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6462165
						process.getInputStream().close();
						process.getOutputStream().close();
						process.getErrorStream().close();
						process.destroy();
					}
					if (tempFile != null && !tempFile.delete()) {
						tempFile.deleteOnExit();
					}
				}
			} catch (final InterruptedException e) {
				pid = e.toString();
				System.setProperty("pid", pid);
			} catch (final IOException e) {
				pid = e.toString();
				System.setProperty("pid", pid);
			}
		}
		return pid;
	}

	private static void pump(InputStream is, OutputStream os, boolean closeIn, boolean closeOut)
			throws IOException {
		try {
			TransportFormat.pump(is, os);
		} finally {
			try {
				if (closeIn) {
					is.close();
				}
			} finally {
				if (closeOut) {
					os.close();
				}
			}
		}
	}
}

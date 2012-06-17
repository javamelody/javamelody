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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Date;
import java.util.Locale;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe JRobin.
 * @author Emeric Vernat
 */
public class TestJRobin {
	private static final String TEST_APPLICATION = "test";

	/** Test. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void test() throws IOException {
		final Range range = Period.JOUR.getRange();
		final Range customRange = Range.createCustomRange(new Date(System.currentTimeMillis() - 24L
				* 60 * 60 * 1000), new Date());
		final JRobin jrobin = JRobin.createInstance(TEST_APPLICATION, "id",
				"requête très très très très très très longue");
		jrobin.graph(range, 500, 200);
		jrobin.graph(range, 80, 80);
		jrobin.graph(customRange, 500, 200);
		jrobin.graph(customRange, 80, 80);

		jrobin.getLastValue();
		jrobin.deleteFile();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testChinese() throws IOException {
		final Range range = Period.JOUR.getRange();
		final Locale locale = Locale.getDefault();
		try {
			Locale.setDefault(Locale.CHINESE);
			final JRobin jrobin = JRobin.createInstance(TEST_APPLICATION, "cpu", null);
			jrobin.graph(range, 80, 80);
			jrobin.deleteFile();
		} finally {
			Locale.setDefault(locale);
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testInitAndResetFile() throws IOException {
		final String application = TEST_APPLICATION;
		final String jrobinName = "name";
		final File dir = Parameters.getStorageDirectory(application);
		// ce fichier sera celui utilisé par JRobin
		final File rrdFile = new File(dir, jrobinName + ".rrd");

		// JRobin.createInstance devrait initialiser les fichiers non existants
		assertTrue("delete", !rrdFile.exists() || rrdFile.delete());
		JRobin.createInstance(application, jrobinName, "request");

		// JRobin.createInstance devrait réinitialiser les fichiers de longueur 0
		new FileOutputStream(rrdFile).close();
		assertEquals("check file length", 0, rrdFile.length());
		JRobin.createInstance(application, jrobinName, "request");

		assertTrue("delete", !rrdFile.exists() || rrdFile.delete());
		final FileOutputStream out = new FileOutputStream(rrdFile);
		try {
			// il faut un minimum de quantité de données pour avoir RrdException "Invalid file header"
			for (int i = 0; i < 100; i++) {
				out.write("n'est pas un fichier rrd".getBytes());
			}
		} finally {
			out.close();
		}
		final JRobin jrobin = JRobin.createInstance(application, jrobinName, "request");
		// addValue devrait appeler resetFile car RrdException "Invalid file header"
		// puis devrait relancer une IOException
		try {
			jrobin.addValue(1);
		} catch (final IOException e) {
			assertTrue("cause", e.getCause() != null && e.getCause().getMessage() != null
					&& e.getCause().getMessage().contains("Invalid file header"));
		}
		// après ce resetFile, on devrait pouvoir appeler addValue
		jrobin.addValue(1);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testDeleteObsoleteJRobinFiles() throws IOException {
		JRobin.deleteObsoleteJRobinFiles(TEST_APPLICATION);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testStop() throws IOException {
		// à défaut de pouvoir appeler JRobin.stop() car les autres tests ne pourront plus
		// utiliser JRobin, on appelle au moins JRobin.getJRobinFileSyncTimer()
		JRobin.getJRobinFileSyncTimer();
	}
}

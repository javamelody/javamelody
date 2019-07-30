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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Date;
import java.util.Locale;
import java.util.Timer;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Test unitaire de la classe JRobin.
 * @author Emeric Vernat
 */
public class TestJRobin {
	private static final String TEST_APPLICATION = "test";

	/** Before.
	 * @throws IOException e */
	@Before
	public void setUp() throws IOException {
		Utils.initialize();
		JRobin.initBackendFactory(new Timer(getClass().getSimpleName(), true));
	}

	/** After. */
	@After
	public void tearDown() {
		JRobin.stop();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void test() throws IOException {
		final Range range = Period.JOUR.getRange();
		final Range customRange = Range.createCustomRange(
				new Date(System.currentTimeMillis() - 24L * 60 * 60 * 1000), new Date());
		final JRobin jrobin = JRobin.createInstance(TEST_APPLICATION, "id",
				"requête très très très très très très longue");
		jrobin.graph(range, 500, 200);
		jrobin.graph(range, 80, 80);
		jrobin.graph(customRange, 500, 200);
		jrobin.graph(customRange, 80, 80);

		jrobin.getLastValue();
		jrobin.getMeanValue(customRange);
		jrobin.deleteFile();
		jrobin.toString();
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
		Utils.setProperty(Parameter.OBSOLETE_GRAPHS_DAYS, "1");
		JRobin.deleteObsoleteJRobinFiles(TEST_APPLICATION);
	}
}

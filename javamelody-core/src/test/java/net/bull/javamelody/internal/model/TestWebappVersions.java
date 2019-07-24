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

import java.io.File;
import java.io.IOException;
import java.util.Random;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Test unitaire de la classe CounterStorage.
 * @author Emeric Vernat
 */
public class TestWebappVersions {
	private static final String VERSIONS_FILENAME = "versions.properties";

	/** Test. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void test() throws IOException {
		final String application = "unittest";
		final File storageDirectory = Parameters.getStorageDirectory(application);
		final File versionsFile = new File(storageDirectory, VERSIONS_FILENAME);
		versionsFile.delete();
		WebappVersions webappVersions = new WebappVersions(application);
		assertEquals("0", 0, webappVersions.getDatesByVersions().size());
		final Random random = new Random();
		final String version = String.valueOf(random.nextInt());
		webappVersions.addVersionIfNeeded(version);
		webappVersions = new WebappVersions(application);
		assertEquals("1", 1, webappVersions.getDatesByVersions().size());
		webappVersions.addVersionIfNeeded(version);
		assertEquals("1", 1, webappVersions.getDatesByVersions().size());
		webappVersions.addVersionIfNeeded(version + "b");
		assertEquals("2", 2, webappVersions.getDatesByVersions().size());
	}
}

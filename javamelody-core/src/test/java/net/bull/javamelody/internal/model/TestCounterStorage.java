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

import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Calendar;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Test unitaire de la classe CounterStorage.
 * @author Emeric Vernat
 */
public class TestCounterStorage {
	/** Test. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testDeleteObsoleteCounterFiles() throws IOException {
		final Counter counter = new Counter("http", null);
		counter.setApplication("test counter");
		final File storageDir = Parameters.getStorageDirectory(counter.getApplication());
		final File obsoleteFile = new File(storageDir, "obsolete.ser.gz");
		final File notObsoleteFile = new File(storageDir, "notobsolete.ser.gz");
		checkSetup(storageDir, obsoleteFile, notObsoleteFile);
		final Calendar nowMinus1YearAnd2Days = Calendar.getInstance();
		nowMinus1YearAnd2Days.add(Calendar.YEAR, -1);
		nowMinus1YearAnd2Days.add(Calendar.DAY_OF_YEAR, -2);
		if (!obsoleteFile.setLastModified(nowMinus1YearAnd2Days.getTimeInMillis())) {
			fail("setLastModified");
		}

		CounterStorage.deleteObsoleteCounterFiles(counter.getApplication());

		// le fichier doit avoir été supprimé
		if (obsoleteFile.exists()) {
			fail("obsolete file still exists");
		}
		if (!notObsoleteFile.delete()) {
			notObsoleteFile.deleteOnExit();
		}

		Utils.setProperty(Parameter.OBSOLETE_STATS_DAYS, "1");
		CounterStorage.deleteObsoleteCounterFiles(counter.getApplication());
	}

	private void checkSetup(final File storageDir, final File obsoleteFile,
			final File notObsoleteFile) throws IOException {
		if (!storageDir.exists() && !storageDir.mkdirs()) {
			fail("mkdir");
		}
		if (!obsoleteFile.exists() && !obsoleteFile.createNewFile()) {
			fail("createNewFile");
		}
		if (!notObsoleteFile.exists() && !notObsoleteFile.createNewFile()) {
			fail("createNewFile");
		}
	}
}

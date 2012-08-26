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

import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Calendar;

import org.junit.Before;
import org.junit.Test;

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

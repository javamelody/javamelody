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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.List;
import java.util.Locale;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;

/**
 * Test unitaire de la classe ProcessInformations.
 * @author Emeric Vernat
 */
public class TestProcessInformations {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testReadPs() {
		final List<ProcessInformations> processInformations = ProcessInformations
				.buildProcessInformations(getClass().getResourceAsStream("/tasklist.txt"), true,
						false);
		assertSame("processes", processInformations.size(), 49);
		checkProcesses(processInformations, true);
		final List<ProcessInformations> processInformations2 = ProcessInformations
				.buildProcessInformations(getClass().getResourceAsStream("/ps.txt"), false, false);
		assertSame("processes", processInformations2.size(), 118);
		checkProcesses(processInformations2, false);
		final List<ProcessInformations> processInformations3 = ProcessInformations
				.buildProcessInformations(getClass().getResourceAsStream("/ps_aix.txt"), false,
						true);
		assertSame("processes", processInformations3.size(), 15);
		checkProcesses(processInformations3, false);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testExecuteAndReadPs() throws IOException {
		final List<ProcessInformations> processes = ProcessInformations.buildProcessInformations();
		assertNotNull("processes null", processes);
		assertFalse("processes vide", processes.isEmpty());
		final boolean windows = System.getProperty("os.name").toLowerCase(Locale.getDefault())
				.contains("windows");
		checkProcesses(processes, windows);
	}

	private void checkProcesses(List<ProcessInformations> processInformations, boolean windows) {
		for (final ProcessInformations process : processInformations) {
			assertNotNull("user", process.getUser());
			assertTrue("pid", process.getPid() >= 0);
			if (!windows) {
				assertTrue("cpuPercentage", process.getCpuPercentage() >= 0);
				assertTrue("memPercentage", process.getMemPercentage() >= 0);
				assertTrue("rss", process.getRss() >= 0);
				assertNotNull("tty", process.getTty());
				assertNotNull("stat", process.getStat());
				assertNotNull("start", process.getStart());
			}
			assertTrue("vsz", process.getVsz() >= 0);
			assertNotNull("cpuTime", process.getCpuTime());
			assertNotNull("command", process.getCommand());
		}
	}
}

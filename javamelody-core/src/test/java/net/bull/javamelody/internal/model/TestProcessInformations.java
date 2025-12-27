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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.util.List;
import java.util.Locale;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import net.bull.javamelody.Utils;

/**
 * Test unitaire de la classe ProcessInformations.
 * @author Emeric Vernat
 */
class TestProcessInformations {
	/** Check. */
	@BeforeEach
	void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	void testReadPs() {
		final List<ProcessInformations> processInformations = ProcessInformations
				.buildProcessInformations(getClass().getResourceAsStream("/tasklist.txt"), true,
						false);
		assertSame(49, processInformations.size(), "processes");
		checkProcesses(processInformations, true);
		final List<ProcessInformations> processInformations2 = ProcessInformations
				.buildProcessInformations(getClass().getResourceAsStream("/ps.txt"), false, false);
		assertSame(118, processInformations2.size(), "processes");
		checkProcesses(processInformations2, false);
		final List<ProcessInformations> processInformations3 = ProcessInformations
				.buildProcessInformations(getClass().getResourceAsStream("/ps_aix.txt"), false,
						true);
		assertSame(15, processInformations3.size(), "processes");
		checkProcesses(processInformations3, false);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	void testExecuteAndReadPs() throws IOException {
		final List<ProcessInformations> processes = ProcessInformations.buildProcessInformations();
		assertNotNull(processes, "processes null");
		assertFalse(processes.isEmpty(), "processes vide");
		final boolean windows = System.getProperty("os.name").toLowerCase(Locale.getDefault())
				.contains("windows");
		checkProcesses(processes, windows);
	}

	private void checkProcesses(List<ProcessInformations> processInformations, boolean windows) {
		for (final ProcessInformations process : processInformations) {
			assertNotNull(process.getUser(), "user");
			assertTrue(process.getPid() >= 0, "pid");
			if (!windows) {
				assertTrue(process.getCpuPercentage() >= 0, "cpuPercentage");
				assertTrue(process.getMemPercentage() >= 0, "memPercentage");
				assertTrue(process.getRss() >= 0, "rss");
				assertNotNull(process.getTty(), "tty");
				assertNotNull(process.getStat(), "stat");
				assertNotNull(process.getStart(), "start");
			}
			assertTrue(process.getVsz() >= 0, "vsz");
			assertNotNull(process.getCpuTime(), "cpuTime");
			assertNotNull(process.getCommand(), "command");
		}
	}
}

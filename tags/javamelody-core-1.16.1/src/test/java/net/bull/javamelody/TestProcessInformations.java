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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.List;

import org.junit.Test;

/**
 * Test unitaire de la classe ProcessInformations.
 * @author Emeric Vernat
 */
public class TestProcessInformations {
	/** Test. */
	@Test
	public void testReadPs() {
		final List<ProcessInformations> processInformations = ProcessInformations
				.buildProcessInformations(getClass().getResourceAsStream("/tasklist.txt"), true);
		assertSame("processes", processInformations.size(), 49);
		checkProcesses(processInformations, true);
		final List<ProcessInformations> processInformations2 = ProcessInformations
				.buildProcessInformations(getClass().getResourceAsStream("/ps.txt"), false);
		assertSame("processes", processInformations2.size(), 118);
		checkProcesses(processInformations2, false);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testExecuteAndReadPs() throws IOException {
		final List<ProcessInformations> processes = ProcessInformations.buildProcessInformations();
		assertNotNull("processes null", processes);
		assertFalse("processes vide", processes.isEmpty());
		checkProcesses(processes, ProcessInformations.WINDOWS);
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

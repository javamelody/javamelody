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
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

/**
 * Test unitaire de la classe HsErrPid.
 * @author Emeric Vernat
 */
public class TestHsErrPid {
	/** Test.
	 * @throws IOException e */
	@Test
	public void testBuildHsErrPidList() throws IOException {
		final List<HsErrPid> hsErrPidList = HsErrPid.buildHsErrPidList();
		final File file = new File("./hs_err_pid12345.log");
		final File file2 = new File("./hs_err_pid67890.log");
		final File file3 = new File("./hs_err_pid12345.notlog");
		try {
			file.createNewFile();
			file2.createNewFile();
			file3.createNewFile();
			final List<HsErrPid> hsErrPidList2 = HsErrPid.buildHsErrPidList();
			assertEquals("buildHsErrPidList", hsErrPidList.size() + 2, hsErrPidList2.size());
			for (final HsErrPid hsErrPid : hsErrPidList2) {
				assertNotNull("getDate", hsErrPid.getDate());
				assertNotNull("getFile", hsErrPid.getFile());
			}
			final JavaInformations javaInformations = new JavaInformations(null, true);
			final JavaInformations javaInformations2 = new JavaInformations(null, false);
			final List<HsErrPid> hsErrPidList3 = HsErrPid
					.getHsErrPidList(Arrays.asList(javaInformations, javaInformations2));
			assertEquals("buildHsErrPidList", hsErrPidList.size() + 2, hsErrPidList3.size());
		} finally {
			file.delete();
			file2.delete();
			file3.delete();
		}
	}
}

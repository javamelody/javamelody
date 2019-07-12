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

import static org.easymock.EasyMock.createNiceMock;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.net.URL;
import java.util.Collections;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Test unitaire de la classe LabradorRetriever.
 * @author Emeric Vernat
 */
public class TestLabradorRetriever {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
		 * @throws IOException e */
	@Test
	public void testCall() throws IOException {
		Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "mockLabradorRetriever", "false");
		final File file = File.createTempFile("test", ".ser");
		try {
			final ObjectOutputStream output = new ObjectOutputStream(new FileOutputStream(file));
			try {
				output.writeObject(new Counter("http", null));
			} finally {
				output.close();
			}
			final URL url = file.toURI().toURL();
			final LabradorRetriever labradorRetriever = new LabradorRetriever(url);
			labradorRetriever.call();
			final Map<String, String> headers = Collections.emptyMap();
			final LabradorRetriever labradorRetriever2 = new LabradorRetriever(url, headers);
			labradorRetriever2.call();
		} finally {
			if (!file.delete()) {
				fail("file.delete");
			}
		}
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testCopyTo() throws IOException {
		Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + "mockLabradorRetriever", "false");
		final File file = File.createTempFile("testLabradorRetriever", null);
		try {
			final HttpServletRequest request = createNiceMock(HttpServletRequest.class);
			final HttpServletResponse response = createNiceMock(HttpServletResponse.class);
			// si le fichier n'était pas vide il faudrait retourner un ByteArrayOutputStream
			// pour response.getOutputStream() en utilisant expect et replay
			new LabradorRetriever(file.toURI().toURL()).copyTo(request, response);
		} finally {
			if (!file.delete()) {
				file.deleteOnExit();
			}
		}
	}
}

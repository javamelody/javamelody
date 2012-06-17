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

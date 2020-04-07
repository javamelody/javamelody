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
package net.bull.javamelody.internal.web.html;

import java.io.IOException;
import java.io.Writer;
import java.security.NoSuchAlgorithmException;
import java.security.Security;
import java.util.TreeSet;

import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.MessageDigestPasswordEncoder;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Rapport html pour calculer le hash d'un mot de passe.
 * @author Emeric Vernat
 */
class HtmlHashPasswordReport extends HtmlAbstractReport {
	HtmlHashPasswordReport(Writer writer) {
		super(writer);
	}

	@Override
	void toHtml() throws IOException {
		throw new UnsupportedOperationException();
	}

	void writeHashPassword(String algorithm, String password) throws IOException {
		writeln("<h1>Hash a password for the authorized-users parameter</h1>");
		if (algorithm != null && password != null) {
			write("<label for='hash'>Hash:</label> ");
			final String hash = encodePassword(algorithm, password);
			writeln("<input type='text' id='hash' value='" + hash + "' size='80' />");
			writeln("<button onclick='copyHash()'>Copy</button>");
			writeln("<script type='text/javascript'>");
			writeln("function copyHash() {");
			writeln("document.getElementById('hash').select();");
			writeln("document.getElementById('hash').setSelectionRange(0, 99999); /*For mobile devices*/");
			writeln("document.execCommand('copy');");
			writeln("}");
			writeln("</script>");
			writeln("<br/><br/>");
		}
		writeln("<form action='' method='post' style='padding: 10px;'>");
		writeln("<label for='algorithm'>Algorithm:</label>");
		writeln("<select name='" + HttpParameter.ALGORITHM.getName()
				+ "' id='algorithm' required>");
		for (final String algo : getSortedAlgorithms()) {
			if (algorithm != null && algo.equals(algorithm)
					|| algorithm == null && "SHA-256".equals(algo)) {
				writeln("<option selected>" + algo + "</option>");
			} else {
				writeln("<option>" + algo + "</option>");
			}
		}
		writeln("</select>");
		writeln("<br/><br/>");
		writeln("<label for='password'>Password:</label>");
		writeln("<input type='password' name='" + HttpParameter.REQUEST.getName()
				+ "' id='password' required />");
		writeln("<br/><br/>");
		writeln("<input type='submit'/>");
		writeln("</form>");
		writeln("<br/>Note that you can also hash a password using the command line: <code>java -cp javamelody-core-"
				+ Parameters.JAVAMELODY_VERSION
				+ ".jar net.bull.javamelody.internal.common.MessageDigestPasswordEncoder passwordToHash</code>");
	}

	private TreeSet<String> getSortedAlgorithms() {
		return new TreeSet<String>(Security.getAlgorithms("MessageDigest"));
	}

	private String encodePassword(String algorithm, String password) throws IOException {
		try {
			return new MessageDigestPasswordEncoder(algorithm).encodePassword(password);
		} catch (final NoSuchAlgorithmException e) {
			throw new IOException(e);
		}
	}
}
